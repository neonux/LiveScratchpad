/* vim:set ts=2 sw=2 sts=2 et: */
/* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is Live Scratchpad code.
 *
 * The Initial Developer of the Original Code is Mozilla Foundation.
 * Portions created by the Initial Developer are Copyright (C) 2012
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *   Cedric Vivier <cedricv@neonux.com> (original author)
 *
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or
 * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
 *
 * ***** END LICENSE BLOCK ***** */

"use strict";

const EXPORTED_SYMBOLS = ["LiveEvaluator"];

const Cu = Components.utils;
Cu.import("chrome://LiveScratchpad/content/esprima.jsm");

// max evaluate frequency in ms
const EVALUATE_THROTTLE_DELAY = 500;

const AbortEvaluationReason = {
  PARSE_ERROR: "PARSE_ERROR",
  NO_FUNCTION: "NO_FUNCTION"
};

const RecorderFunctionNames = {
  BRANCH_EVENT: "__evaluator_branch",
  CALL_EVENT: "__evaluator_call",
  LOOP_EVENT: "__evaluator_loop",
  RETURN_EVENT: "__evaluator_return",
  VARIABLE_EVENT: "__evaluator_var"
};

const VariableEventType = {
  ASSIGNMENT: "assignment",
  DECLARATION: "declaration",
  UPDATE: "update"
};

const LoopEventType = {
  ENTER: "enter",
  ITERATION: "iteration",
  LEAVE: "leave"
};

/**
 * LiveEvaluator constructor.
 *
 * This provides a way to evaluate a JavaScript function live as it is being
 * being edited in a SourceEditor.
 * Events occurring during the evaluation, such as assignments, are observable.
 *
 * @see editor
 * @see addObserver
 */
function LiveEvaluator()
{
  this._editor = null;
  this._args = [];

  this._sandboxFactory = null;

  this._ast = null;
  this._observers = [];

  this._evaluateBinding = this._evaluate.bind(this);
  this._onTextChangeBinding = this._onTextChange.bind(this);

  this._instrumenterVisitor = new InstrumenterASTVisitor();
  this._printerVisitor = new PrinterASTVisitor();
}

LiveEvaluator.prototype = {
  /**
   * Retrieve the SourceEditor instance attached to this evaluator.
   */
  get editor() this._editor,

  /**
   * Set the SourceEditor instance attached to this evaluator.
   *
   * @param SourceEditor aEditor
   */
  set editor(aEditor)
  {
    if (this._editor == aEditor) {
      return;
    }
    if (this._editor) {
      this._editor.removeEventListener(this._editor.EVENTS.TEXT_CHANGED,
                                       this._onTextChangeBinding);
    }

    this._editor = aEditor;

    if (this._editor) {
      this._window = this._editor.editorElement.ownerDocument.defaultView;
      this._editor.addEventListener(this._editor.EVENTS.TEXT_CHANGED,
                                    this._onTextChangeBinding);
    }
  },

  /**
   * Retrieve the sandbox factory function.
   * Calling this function returns a new sandbox.
   *
   * @return function
   */
  get sandboxFactory() this._sandboxFactory,

  /**
   * Set the sandbox factory function.
   *
   * @param function aSandboxFactory
   */
  set sandboxFactory(aSandboxFactory)
  {
    this._sandboxFactory = aSandboxFactory;
  },

  /**
   * Called when the attached editor's text changed.
   */
  _onTextChange: function LE__onTextChange(aEvent)
  {
    this.evaluate();
  },

  /**
   * Set an argument expression for the next evaluation.
   *
   * @param number aIndex
   *        The zero-based index of the argument to set.
   * @param {string} aValue
   *        The argument expression.
   */
  setArgument: function LE_setArgument(aIndex, aValue)
  {
    if (aValue === undefined || aValue === "") {
      aValue = "undefined";
    }
    this._args[aIndex] = aValue.toString();
  },

  /**
   * Reset evaluation arguments to none.
   */
  resetArguments: function LE_resetArguments()
  {
    this._args = [];
  },

  // TODO: evaluateExpression() for key in "." => autocomplete at rangeEnd

  /**
   * Evaluate the function at current caret offset.
   * The function is throttled to execute at most twice per second.
   *
   * An observer will get notified of the start of an evaluation through the
   * 'StartEvaluation' observer method, or 'AbortEvaluation' if no evaluation
   * could be started (eg. there is no function to evaluate at the current caret
   * offset).
   *
   * @param {boolean} aImmediate
   *        Optional. If true, the call is not throttled.
   * @see setArgument
   * @see resetArguments
   */
  evaluate: function LE_evaluate(aImmediate)
  {
    if (!this.editor) {
      return;
    }

    if (this._evaluateTask) {
      // cancel previous queued task not executed within throttle delay
      this._window.clearTimeout(this._evaluateTask);
    }

    if (aImmediate) {
      this._evaluate();
    } else {
      this._evaluateTask = this._window.setTimeout(this._evaluateBinding,
                                             EVALUATE_THROTTLE_DELAY);
    }
  },

  _evaluate: function LE__evaluate()
  {
    if (!this.editor) {
      return;
    }

    if (!this._parse()) {
      this._triggerObservers("AbortEvaluation",
                             [AbortEvaluationReason.PARSE_ERROR,
                              this._parseError]);
      return;
    }

    let func = this._getFunctionAtOffset();
    if (!func) {
      //FIXME: we should try to detect the node of the last evaluated function
      //       if available, this would enable live-evaluation while changing
      //       constants and such in outer scopes.
      this._triggerObservers("AbortEvaluation",
                             [AbortEvaluationReason.NO_FUNCTION]);
      return;
    }

    // instrument the AST and generate instrumented source string from it
    this._instrumenterVisitor.visit(func);
    if (this._instrumenterVisitor.hasInfiniteLoop) {
      /* silently abort, it is likely the user is editing a for statement now */
      return false;
    }
    this._printerVisitor.visit(this._ast);
    let source = this._printerVisitor.toString();
    // append actual call to the function we instrumented
    source += "\n" + func.id.name + "(" + this._args.join(",") + ");";

    // grab a new sandbox and import the recorder functions into it
    let sandbox = this.sandboxFactory();
    this._importRecorderFunctionsIntoSandbox(sandbox);

    // evaluate the instrumented source in the sandbox
    this._triggerObservers("StartEvaluation", [func]);
    try {
      Cu.evalInSandbox(source, sandbox, "1.8", "LiveScratchpad", 1);
    } catch (ex) {
      this._triggerObservers("UnhandledException", [ex]);
    } finally {
      this._triggerObservers("StopEvaluation", [func]);
    }
  },

  /**
   * Import recorder functions into the given sandbox.
   *
   * @param object aSandbox
   */
  _importRecorderFunctionsIntoSandbox: function LE__importRecorderFunctionsIntoSandbox(aSandbox)
  {
    aSandbox.importFunction(function __var(aValue, aRangeStart, aRangeEnd, aEventType, aName) {
      this._triggerObservers("VariableEvent",
                             [aValue, aRangeStart, aRangeEnd, aEventType, aName]);
      return aValue;
    }.bind(this), RecorderFunctionNames.VARIABLE_EVENT);
    aSandbox.importFunction(function __return(aValue, aRangeStart, aRangeEnd) {
      this._triggerObservers("ReturnEvent",
                             [aValue, aRangeStart, aRangeEnd]);
      return aValue;
    }.bind(this), RecorderFunctionNames.RETURN_EVENT);
  },

  /**
   * Add an observer for LiveEvaluator events.
   *
   * The observer implements ILiveEvaluatorObserver := {
   *   onStartEvaluation:      Called when an evaluation starts.
   *                           Arguments: (LiveEvaluator aEvaluator)
   *
   *   onStopEvaluation:       Called when a new evaluation stops.
   *                           Arguments: (LiveEvaluator aEvaluator)
   *
   *   onAbortEvaluation:      Called when an evaluation could not start.
   *                           Arguments: (LiveEvaluator aEvaluator,
   *                                       AbortEvaluationReason aReason,
   *                                       {Error} aError)
   *
   *   onVariableEvent:        Called when a variable gets assigned to.
   *                           Arguments: (LiveEvaluator aEvaluator,
   *                                       any aValue,
   *                                       Number aRangeStart,
   *                                       Number aRangeEnd,
   *                                       string aEventType,
   *                                       string aVariableName)
   *
   *   onReturnEvent:          Called when the evaluated function returns.
   *                           Arguments: (LiveEvaluator evaluator,
   *                                       any aValue,
   *                                       Number aRangeStart,
   *                                       Number aRangeEnd)
   * }
   *
   * All observer methods are optional.
   *
   * @param ILiveEvaluatorObserver aObserver
   * @see removeObserver
   */
  addObserver: function LE_addObserver(aObserver)
  {
    this._observers.push(aObserver);
  },

  /**
   * Remove an observer from the current list of observers.
   *
   * @param ILiveEvaluatorObserver aObserver
   */
  removeObserver: function LE_removeObserver(aObserver)
  {
    let index = this._observers.indexOf(aObserver);
    if (index != -1) {
      this._observers.splice(index, 1);
    }
  },

  /**
   * Trigger a named observer method in all observers.
   *
   * @param string aName
   *        The method name to trigger.
   * @param {Array} aArgs
   *        Optional array of additional arguments to call the method with.
   */
  _triggerObservers: function LE__triggerObservers(aName, aArgs)
  {
    // insert the origin instance as first argument
    if (!aArgs) {
      aArgs = [this];
    } else {
      aArgs.unshift(this);
    }

    // copy the list of observers to allow addition/removal in handlers
    let observers = this._observers.concat();
    // trigger all observers that have this handler
    for (let i = 0; i < observers.length; ++i) {
      let observer = observers[i];
      let handler = observer["on" + aName];
      if (handler) {
        handler.apply(observer, aArgs);
      }
    }
  },

  /**
   * Parse source in the editor and generate an AST.
   *
   * @return boolean
   *         Returns true if successfully parsed, false otherwise.
   */
  _parse: function LE__parse()
  {
    try {
      this._ast = esprima.parse(this.editor.getText(),
                                {loc: true, range: true, tolerant: true});
      return true;
    } catch (ex) {
      this._parseError = ex;
      this._ast = null;
      return false;
    }
  },

  /**
   * Retrieve the deepest function at the given offset in the AST.
   *
   * @param {number} aOffset
   *        Optional offset. If undefined, the current caret offset is used.
   * @return object
   *         FunctionDeclaration node or null if not found.
   * @see parse
   */
  _getFunctionAtOffset: function LE_getFunctionAtOffset(aOffset)
  {
    aOffset = aOffset || this.editor.getCaretOffset();
    return this._findDeepestAtOffset("FunctionDeclaration", aOffset);
  },

  /**
   * Find the deepest node of a given type present at given offset.
   *
   * @param string aType
   *        The type of node to look for.
   * @param {number} aOffset
   *        Optional offset. If undefined, use the current caret offset.
   * @return object
   *         Node of requested type or null if not found.
   */
  _findDeepestAtOffset: function LE__findFunctionAtOffset(aType, aOffset)
  {
    aOffset = aOffset || this.editor.getCaretOffset();
    let func = null;
    this._traverseNodes(function (aNode) {
      if (!aNode.range) {
        return; // pass through synthetic nodes
      }
      if (aNode.range[1] < aOffset) {
        return false; // do not traverse children, this node ends before offset
      }
      if (aNode.range[0] > aOffset) {
        throw StopIteration; // this node starts after offset, stop it!
      }
      if (aNode.type == aType
          && aNode.range[0] <= aOffset && aNode.range[1] >= aOffset) {
        func = aNode;
      }
    });
    return func;
  },

  /**
   * Traverse nodes in the AST from given root node.
   * FIXME: remove this, reuse visitor instead => NodeFinderVisitor
   * TODO: NodeFinderVisitor could perform a binary search for best performance
   *
   * @param function aCallback
   * @param object {aRootNode}
   */
  _traverseNodes: function LE__traverseNodes(aCallback, aRootNode, _aLevel)
  {
    let node = aRootNode || this._ast;
    try {
      if (aCallback(node, _aLevel || 0) !== false) {
        let level = !_aLevel ? 1 : _aLevel + 1;
        for (let prop in node) {
          let childNode = node[prop];
          if (!childNode) {
            continue;
          }
          if (childNode.length !== undefined) {
            for each (let child in childNode) {
              if (child.type) {
                this._traverseNodes(aCallback, child, level);
              }
            }
          } else if (childNode.type) {
            this._traverseNodes(aCallback, childNode, level);
          }
        }
      }
    } catch (e if e instanceof StopIteration && _aLevel) {
      throw e; // rethrow to propagate stop, up to level 0
    } catch (e if e instanceof StopIteration) {
      // swallow at level 0, exit
    }
  }
};

/*              */
/* AST visitors */

/**
 * ASTVisitor constructor.
 *
 * This object allows to visit an AST as generated by LiveEvaluator._parse
 * through callback functions for each AST node type (on<node.type>).
 * Statements and expressions with a body call enter<node.type> and
 * leave<node.type> callbacks if available. If enter<node.type> returns false,
 * the node body is not visited.
 *
 * @param object aVisitor
 *        Object that optionally contains callbacks for each AST node type.
 */
function ASTVisitor(aVisitor)
{
  this._visitor = aVisitor;
}

//TODO: meta-program this?
ASTVisitor.prototype =
{
  /**
   * Visit an abstrast syntax tree starting from a given node.
   *
   * @param object aNode
   *        Node to start the visit from (possibly the root 'Program' node).
   */
  visit: function ASTV_visit(aNode)
  {
    if (!aNode) {
      return;
    }
    if (aNode.length !== undefined) {
      for each (let node in aNode) {
        if (node) {
          let replacement = node.$replacement;
          if (replacement) {
            node.$replacement = null;
            node = replacement;
          }
          this["on" + node.type](node);
          if (replacement) {
            node.$replacement = replacement;
          }
        }
      }
    } else {
      let replacement = aNode.$replacement;
      if (replacement) {
        aNode.$replacement = null;
        aNode = replacement;
      }
      this["on" + aNode.type](aNode);
      if (replacement) {
        aNode.$replacement = replacement;
      }
    }
  },

  onProgram: function (aNode) {
    if (this._visitor.enterProgram) {
      if (this._visitor.enterProgram(aNode) === false) {
        return;
      }
    }
    if (this._visitor.onProgram) {
      this._visitor.onProgram(aNode);
    }

    this.visit(aNode.body);

    if (this._visitor.leaveProgram) {
      this._visitor.leaveProgram(aNode);
    }
  },

  onEmptyStatement: function (aNode) {
    if (this._visitor.onEmptyStatement) {
      this._visitor.onEmptyStatement(aNode);
    }
  },

  onBlockStatement: function (aNode) {
    if (this._visitor.enterBlockStatement) {
      if (this._visitor.enterBlockStatement(aNode) === false) {
        return;
      }
    }
    if (this._visitor.onBlockStatement) {
      this._visitor.onBlockStatement(aNode);
    }

    this.visit(aNode.body);

    if (this._visitor.leaveBlockStatement) {
      this._visitor.leaveBlockStatement(aNode);
    }
  },

  onExpressionStatement: function (aNode) {
    if (this._visitor.enterExpressionStatement) {
      if (this._visitor.enterExpressionStatement(aNode) === false) {
        return;
      }
    }
    if (this._visitor.onExpressionStatement) {
      this._visitor.onExpressionStatement(aNode);
    }

    this.visit(aNode.expression);

    if (this._visitor.leaveExpressionStatement) {
      this._visitor.leaveExpressionStatement(aNode);
    }
  },

  onIfStatement: function (aNode) {
    if (this._visitor.enterIfStatement) {
      if (this._visitor.enterIfStatement(aNode) === false) {
        return;
      }
    }
    if (this._visitor.onIfStatement) {
      this._visitor.onIfStatement(aNode);
    }

    this.visit(aNode.test);
    this.visit(aNode.consequent);
    this.visit(aNode.alternate);

    if (this._visitor.leaveIfStatement) {
      this._visitor.leaveIfStatement(aNode);
    }
  },

  onLabeledStatement: function (aNode) {
  },

  onBreakStatement: function (aNode) {
    if (this._visitor.onBreakStatement) {
      this._visitor.onBreakStatement(aNode);
    }
  },

  onContinueStatement: function (aNode) {
    if (this._visitor.onContinueStatement) {
      this._visitor.onContinueStatement(aNode);
    }
  },

  onSwitchStatement: function (aNode) {
    if (this._visitor.enterSwitchStatement) {
      if (this._visitor.enterSwitchStatement(aNode) === false) {
        return;
      }
    }
    if (this._visitor.onSwitchStatement) {
      this._visitor.onSwitchStatement(aNode);
    }

    this.visit(aNode.discriminant);
    this.visit(aNode.cases);

    if (this._visitor.leaveSwitchStatement) {
      this._visitor.leaveSwitchStatement(aNode);
    }
  },

  onReturnStatement: function (aNode) {
    if (this._visitor.enterReturnStatement) {
      if (this._visitor.enterReturnStatement(aNode) === false) {
        return;
      }
    }
    if (this._visitor.onReturnStatement) {
      this._visitor.onReturnStatement(aNode);
    }

    this.visit(aNode.argument);

    if (this._visitor.leaveReturnStatement) {
      this._visitor.leaveReturnStatement(aNode);
    }
  },

  onThrowStatement: function (aNode) {
    if (this._visitor.enterThrowStatement) {
      if (this._visitor.enterThrowStatement(aNode) === false) {
        return;
      }
    }
    if (this._visitor.onThrowStatement) {
      this._visitor.onThrowStatement(aNode);
    }

    this.visit(aNode.argument);

    if (this._visitor.leaveThrowStatement) {
      this._visitor.leaveThrowStatement(aNode);
    }
  },

  onTryStatement: function (aNode) {
    if (this._visitor.enterTryStatement) {
      if (this._visitor.enterTryStatement(aNode) === false) {
        return;
      }
    }
    if (this._visitor.onTryStatement) {
      this._visitor.onTryStatement(aNode);
    }

    this.visit(aNode.block);
    this.visit(aNode.handlers);
    this.visit(aNode.finalizer);

    if (this._visitor.leaveTryStatement) {
      this._visitor.leaveTryStatement(aNode);
    }
  },

  onWhileStatement: function (aNode) {
  },

  onDoWhileStatement: function (aNode) {
  },

  onForStatement: function (aNode) {
    if (this._visitor.enterForStatement) {
      if (this._visitor.enterForStatement(aNode) === false) {
        return;
      }
    }
    if (this._visitor.onForStatement) {
      this._visitor.onForStatement(aNode);
    }

    this.visit(aNode.init);
    this.visit(aNode.test);
    this.visit(aNode.update);
    this.visit(aNode.body);

    if (this._visitor.leaveForStatement) {
      this._visitor.leaveForStatement(aNode);
    }
  },

  onForInStatement: function (aNode) {
    if (this._visitor.enterForInStatement) {
      if (this._visitor.enterForInStatement(aNode) === false) {
        return;
      }
    }
    if (this._visitor.onForInStatement) {
      this._visitor.onForInStatement(aNode);
    }

    this.visit(aNode.left);
    this.visit(aNode.right);
    this.visit(aNode.body);

    if (this._visitor.leaveForInStatement) {
      this._visitor.leaveForInStatement(aNode);
    }
  },

  onLetStatement: function (aNode) {
  },

  onDebuggerStatement: function (aNode) {
    if (this._visitor.onDebuggerStatement) {
      this._visitor.onDebuggerStatement(aNode);
    }
  },

  onFunctionDeclaration: function (aNode) {
    if (this._visitor.enterFunctionDeclaration) {
      if (this._visitor.enterFunctionDeclaration(aNode) === false) {
        return;
      }
    }
    if (this._visitor.onFunctionDeclaration) {
      this._visitor.onFunctionDeclaration(aNode);
    }

    this.visit(aNode.params);
    this.visit(aNode.body);

    if (this._visitor.leaveFunctionDeclaration) {
      this._visitor.leaveFunctionDeclaration(aNode);
    }
  },

  onVariableDeclaration: function (aNode) {
    if (this._visitor.enterVariableDeclaration) {
      if (this._visitor.enterVariableDeclaration(aNode) === false) {
        return;
      }
    }
    if (this._visitor.onVariableDeclaration) {
      this._visitor.onVariableDeclaration(aNode);
    }

    this.visit(aNode.declarations);

    if (this._visitor.leaveVariableDeclaration) {
      this._visitor.leaveVariableDeclaration(aNode);
    }
  },

  onVariableDeclarator: function (aNode) {
    if (this._visitor.enterVariableDeclarator) {
      if (this._visitor.enterVariableDeclarator(aNode) === false) {
        return;
      }
    }
    if (this._visitor.onVariableDeclarator) {
      this._visitor.onVariableDeclarator(aNode);
    }

    this.visit(aNode.init);

    if (this._visitor.leaveVariableDeclarator) {
      this._visitor.leaveVariableDeclarator(aNode);
    }
  },

  onCallExpression: function (aNode) {
    if (this._visitor.enterCallExpression) {
      if (this._visitor.enterCallExpression(aNode) === false) {
        return;
      }
    }
    if (this._visitor.onCallExpression) {
      this._visitor.onCallExpression(aNode);
    }

    this.visit(aNode.callee);
    this.visit(aNode.arguments);

    if (this._visitor.leaveCallExpression) {
      this._visitor.leaveCallExpression(aNode);
    }
  },

  onThisExpression: function (aNode) {
    if (this._visitor.onThisExpression) {
      this._visitor.onThisExpression(aNode);
    }
  },

  onArrayExpression: function (aNode) {
    if (this._visitor.enterArrayExpression) {
      if (this._visitor.enterArrayExpression(aNode) === false) {
        return;
      }
    }
    if (this._visitor.onArrayExpression) {
      this._visitor.onArrayExpression(aNode);
    }

    this.visit(aNode.elements);

    if (this._visitor.leaveArrayExpression) {
      this._visitor.leaveArrayExpression(aNode);
    }
  },

  onObjectExpression: function (aNode) {
    if (this._visitor.enterObjectExpression) {
      if (this._visitor.enterObjectExpression(aNode) === false) {
        return;
      }
    }
    if (this._visitor.onObjectExpression) {
      this._visitor.onObjectExpression(aNode);
    }

    this.visit(aNode.properties);

    if (this._visitor.leaveObjectExpression) {
      this._visitor.leaveObjectExpression(aNode);
    }
  },

  onProperty: function (aNode) {
    if (this._visitor.enterProperty) {
      if (this._visitor.enterProperty(aNode) === false) {
        return;
      }
    }
    if (this._visitor.onProperty) {
      this._visitor.onProperty(aNode);
    }

    this.visit(aNode.key);
    this.visit(aNode.value);

    if (this._visitor.leaveProperty) {
      this._visitor.leaveProperty(aNode);
    }
  },

  onFunctionExpression: function (aNode) {
    if (this._visitor.enterFunctionExpression) {
      if (this._visitor.enterFunctionExpression(aNode) === false) {
        return;
      }
    }
    if (this._visitor.onFunctionExpression) {
      this._visitor.onFunctionExpression(aNode);
    }

    this.visit(aNode.params);
    this.visit(aNode.body);

    if (this._visitor.leaveFunctionExpression) {
      this._visitor.leaveFunctionExpression(aNode);
    }
  },

  onUnaryExpression: function (aNode) {
    if (this._visitor.enterUnaryExpression) {
      if (this._visitor.enterUnaryExpression(aNode) === false) {
        return;
      }
    }
    if (this._visitor.onUnaryExpression) {
      this._visitor.onUnaryExpression(aNode);
    }

    this.visit(aNode.argument);

    if (this._visitor.leaveUnaryExpression) {
      this._visitor.leaveUnaryExpression(aNode);
    }
  },

  onBinaryExpression: function (aNode) {
    if (this._visitor.enterBinaryExpression) {
      if (this._visitor.enterBinaryExpression(aNode) === false) {
        return;
      }
    }
    if (this._visitor.onBinaryExpression) {
      this._visitor.onBinaryExpression(aNode);
    }

    this.visit(aNode.left);
    this.visit(aNode.right);

    if (this._visitor.leaveBinaryExpression) {
      this._visitor.leaveBinaryExpression(aNode);
    }
  },

  onAssignmentExpression: function (aNode) {
    if (this._visitor.enterAssignmentExpression) {
      if (this._visitor.enterAssignmentExpression(aNode) === false) {
        return;
      }
    }
    if (this._visitor.onAssignmentExpression) {
      this._visitor.onAssignmentExpression(aNode);
    }

    this.visit(aNode.left);
    this.visit(aNode.right);

    if (this._visitor.leaveAssignmentExpression) {
      this._visitor.leaveAssignmentExpression(aNode);
    }
  },

  onUpdateExpression: function (aNode) {
    if (this._visitor.enterUpdateExpression) {
      if (this._visitor.enterUpdateExpression(aNode) === false) {
        return;
      }
    }
    if (this._visitor.onUpdateExpression) {
      this._visitor.onUpdateExpression(aNode);
    }

    this.visit(aNode.argument);

    if (this._visitor.leaveUpdateExpression) {
      this._visitor.leaveUpdateExpression(aNode);
    }
  },

  onLogicalExpression: function (aNode) {
    if (this._visitor.enterLogicalExpression) {
      if (this._visitor.enterLogicalExpression(aNode) === false) {
        return;
      }
    }
    if (this._visitor.onLogicalExpression) {
      this._visitor.onLogicalExpression(aNode);
    }

    this.visit(aNode.left);
    this.visit(aNode.right);

    if (this._visitor.leaveLogicalExpression) {
      this._visitor.leaveLogicalExpression(aNode);
    }
  },

  onNewExpression: function (aNode) {
    if (this._visitor.enterNewExpression) {
      if (this._visitor.enterNewExpression(aNode) === false) {
        return;
      }
    }
    if (this._visitor.onNewExpression) {
      this._visitor.onNewExpression(aNode);
    }

    this.visit(aNode.callee);
    this.visit(aNode.arguments);

    if (this._visitor.leaveNewExpression) {
      this._visitor.leaveNewExpression(aNode);
    }
  },

  onMemberExpression: function (aNode) {
    if (this._visitor.enterMemberExpression) {
      if (this._visitor.enterMemberExpression(aNode) === false) {
        return;
      }
    }
    if (this._visitor.onMemberExpression) {
      this._visitor.onMemberExpression(aNode);
    }

    this.visit(aNode.object);
    this.visit(aNode.property);

    if (this._visitor.leaveMemberExpression) {
      this._visitor.leaveMemberExpression(aNode);
    }
  },

  onYieldExpression: function (aNode) {
  },

  onGeneratorExpression: function (aNode) {
  },

  onSwitchCase: function (aNode) {
    if (this._visitor.enterSwitchCase) {
      if (this._visitor.enterSwitchCase(aNode) === false) {
        return;
      }
    }
    if (this._visitor.onSwitchCase) {
      this._visitor.onSwitchCase(aNode);
    }

    this.visit(aNode.test);
    this.visit(aNode.consequent);

    if (this._visitor.leaveSwitchCase) {
      this._visitor.leaveSwitchCase(aNode);
    }
  },

  onCatchClause: function (aNode) {
    if (this._visitor.enterCatchClause) {
      if (this._visitor.enterCatchClause(aNode) === false) {
        return;
      }
    }
    if (this._visitor.onCatchClause) {
      this._visitor.onCatchClause(aNode);
    }

    this.visit(aNode.param);
    this.visit(aNode.guard);
    this.visit(aNode.body);

    if (this._visitor.leaveCatchClause) {
      this._visitor.leaveCatchClause(aNode);
    }
  },

  onIdentifier: function (aNode) {
    if (this._visitor.onIdentifier) {
      this._visitor.onIdentifier(aNode);
    }
  },

  onLiteral: function (aNode) {
    if (this._visitor.onLiteral) {
      this._visitor.onLiteral(aNode);
    }
  }
};

/**
 * PrinterASTVisitor constructor.
 *
 * This object allows to generate source code for the AST nodes it visits.
 * The source is generated via a string buffer that accumulates until the source
 * is retrieved using toString.
 * Generated source contains new lines according to the original node locations.
 *
 * @see toString
 */
function PrinterASTVisitor()
{
  this._visitor = new ASTVisitor(this);
  this._reset();
}

PrinterASTVisitor.prototype =
{
  /**
   * Visit an abstrast syntax tree starting from a given node.
   *
   * @param object aNode
   * @param {string} _aToken
   *        Separator to use between nodes (internal).
   * @see ASTVisitor.visit
   */
  visit: function PAV_visit(aNode, _aToken)
  {
    if (!aNode) {
      return;
    }

    if (!_aToken || aNode.length === undefined) {
      while (aNode.loc && this._line < aNode.loc.start.line) {
        this._buffer.push("\n");
        this._line++;
      }
      return this._visitor.visit(aNode);
    }

    for (let i = 0; i < aNode.length; ++i) {
      let node = aNode[i];
      if (!node) {
        continue;
      }
      while (node.loc && this._line < node.loc.start.line) {
        this._buffer.push("\n");
        this._line++;
      }
      this._visitor.visit(node);
      if (i != aNode.length - 1) {
        this._buffer.push(_aToken);
      }
    }
  },

  /**
   * Retrieve the generated source and clear the string buffer.
   *
   * @return string
   */
  toString: function PAV_toString()
  {
    let result = this._buffer.join(" ");
    this._reset();
    return result;
  },

  /**
   * Reset string buffer.
   */
  _reset: function PAV__reset()
  {
    this._line = 1;
    this._buffer = [];
  },

  /*                      */
  /* ASTVisitor callbacks */

  leaveEmptyStatement: function (aNode) {
    //TODO: replace direct pushes with a pushToken function
    //      this would allow selecting minified or pretty print in toString
    this._buffer.push(";");
  },

  leaveExpressionStatement: function (aNode) {
    this._buffer.push(";");
  },

  enterBlockStatement: function (aNode) {
    this._buffer.push("{");
  },
  leaveBlockStatement: function (aNode) {
    this._buffer.push("}");
  },

  onBreakStatement: function (aNode) {
    this._buffer.push("break");
    this._buffer.push(";");
  },

  onDebuggerStatement: function (aNode) {
    this._buffer.push("debugger");
    this._buffer.push(";");
  },

  enterForStatement: function (aNode) {
    this._buffer.push("for");
    this._buffer.push("(");
    if (aNode.init) {
      this.visit(aNode.init);
    } else {
      this._buffer.push(";");
    }
    this.visit(aNode.test);
    this._buffer.push(";");
    this.visit(aNode.update);
    this._buffer.push(")");
    this.visit(aNode.body);
    return false;
  },

  enterForInStatement: function (aNode) {
    this._buffer.push("for");
    this._buffer.push("(");
    this.visit(aNode.left);
    this._buffer.push("in");
    this.visit(aNode.right);
    this._buffer.push(")");
    this.visit(aNode.body);
    return false;
  },

  enterIfStatement: function (aNode) {
    this._buffer.push("if");
    this._buffer.push("(");
    this.visit(aNode.test);
    this._buffer.push(")");
    this.visit(aNode.consequent);
    if (aNode.alternate) {
      this._buffer.push("else");
      this.visit(aNode.alternate);
    }
    return false;
  },

  enterReturnStatement: function (aNode) {
    this._buffer.push("return");
  },
  leaveReturnStatement: function (aNode) {
    this._buffer.push(";");
  },

  enterSwitchStatement: function (aNode) {
    this._buffer.push("switch (");
    this.visit(aNode.discriminant);
    this._buffer.push("{");
    this.visit(aNode.cases);
    this._buffer.push("}");
    return false;
  },

  enterThrowStatement: function (aNode) {
    this._buffer.push("throw");
  },
  leaveThrowStatement: function (aNode) {
    this._buffer.push(";");
  },

  enterTryStatement: function (aNode) {
    this._buffer.push("try");
  },

  enterFunctionDeclaration: function (aNode) {
    this._buffer.push("function");
    this.visit(aNode.id);
    this._buffer.push("(");
    this.visit(aNode.params, ",");
    this._buffer.push(")");
    this.visit(aNode.body);
    return false;
  },

  enterFunctionExpression: function (aNode) {
    return this.enterFunctionDeclaration(aNode);
  },

  enterVariableDeclaration: function (aNode) {
    this._buffer.push(aNode.kind);
    this.visit(aNode.declarations, ",");
    this._buffer.push(";");
    return false;
  },

  onVariableDeclarator: function (aNode) {
    this._buffer.push(aNode.id.name);
    if (aNode.init) {
      this._buffer.push("=");
    }
  },

  enterUnaryExpression: function (aNode) {
    switch (aNode.operator) {
    case "typeof":
      this._buffer.push("typeof");
      this._buffer.push("(");
      break;
    default:
      this._buffer.push(aNode.operator);
      break;
    }
  },
  leaveUnaryExpression: function (aNode) {
    switch (aNode.operator) {
    case "typeof":
      this._buffer.push(")");
      break;
    }
  },

  enterUpdateExpression: function (aNode) {
    if (aNode.prefix) {
      this._buffer.push(aNode.operator);
    }
  },
  leaveUpdateExpression: function (aNode) {
    if (!aNode.prefix) {
      this._buffer.push(aNode.operator);
    }
  },

  enterLogicalExpression: function (aNode) {
    this.visit(aNode.left);
    this._buffer.push(aNode.operator);
    this.visit(aNode.right);
    return false;
  },

  enterAssignmentExpression: function (aNode) {
    this.visit(aNode.left);
    this._buffer.push(aNode.operator);
    this.visit(aNode.right);
    return false;
  },

  enterBinaryExpression: function (aNode) {
    this.visit(aNode.left);
    this._buffer.push(aNode.operator);
    this.visit(aNode.right);
    return false;
  },

  enterCallExpression: function (aNode) {
    this.visit(aNode.callee);
    this._buffer.push("(");
    this.visit(aNode.arguments, ",");
    this._buffer.push(")");
    return false;
  },

  enterMemberExpression: function (aNode) {
    this.visit(aNode.object);
    this._buffer.push(aNode.computed ? "[" : ".");
    this.visit(aNode.property);
    if (aNode.computed) {
      this._buffer.push("]");
    }
    return false;
  },

  enterNewExpression: function (aNode) {
    this._buffer.push("new");
    this.enterCallExpression(aNode);
    return false;
  },

  onThisExpression: function (aNode) {
    this._buffer.push("this");
  },

  enterArrayExpression: function (aNode) {
    this._buffer.push("[");
    this.visit(aNode.elements, ",");
    this._buffer.push("]");
    return false;
  },

  enterObjectExpression: function (aNode) {
    this._buffer.push("{");
    this.visit(aNode.properties, ",");
    this._buffer.push("}");
    return false;
  },

  enterProperty: function (aNode) {
    this.visit(aNode.key);
    this._buffer.push(":");
    this.visit(aNode.value);
    return false;
  },

  enterSwitchCase: function (aNode) {
    this._buffer.push("case");
    this.visit(aNode.test);
    this._buffer.push(":");
    this.visit(aNode.consequent);
    return false;
  },

  enterCatchClause: function (aNode) {
    this._buffer.push("catch");
    this._buffer.push("(");
    this.visit(aNode.param);
    this._buffer.push(")");
    this.visit(aNode.body);
    return false;
  },

  onIdentifier: function (aNode) {
    this._buffer.push(aNode.name);
  },

  onLiteral: function (aNode) {
    let value = aNode.value;
    let isString = typeof(value) == "string";
    if (!isString) {
      if (value === null) {
        this._buffer.push("null");
      } else if (value === undefined) {
        this._buffer.push("undefined");
      } else {
        this._buffer.push(value.toString());
      }
    } else {
      //FIXME: escape?
      this._buffer.push('"' + value + '"');
    }
  }
};

/**
 * InstrumenterASTVisitor constructor.
 * This object allows to instrument the AST to wrap expressions of interests
 * with their corresponding evaluator recorder functions.
 *
 * @see RecorderFunctionNames
 */
function InstrumenterASTVisitor()
{
  this._visitor = new ASTVisitor(this);
}

InstrumenterASTVisitor.prototype =
{
  /**
   * Visit an abstrast syntax tree starting from a given node.
   *
   * @param object aNode
   * @see ASTVisitor.visit
   */
  visit: function IAV_visit(aNode)
  {
    this._hasInfiniteLoop = false;
    return this._visitor.visit(aNode);
  },

  /**
   * Returns true if the last visit encountered an infinite loop.
   *
   * @return boolean
   */
  get hasInfiniteLoop() this._hasInfiniteLoop,

  /**
   * Wrap an expression into a recorder function call expression.
   *
   * @param object aExpression
   *        The AST node of the wrapped expression.
   * @param object aParentNode
   *        The parent AST node of the expression.
   * @param string aRecorder
   *        The name of the recorder function to wrap the expression into.
   * @param {Array} aArgs
   *        Optional additional arguments to the function call.
   *        Every argument must be either a primitive either an AST node.
   * @see RecorderFunctionNames
   */
  _wrapExpression: function IAV__wrapExpression(aExpression, aParentNode, aRecorder, aArgs)
  {
    let args = [
      aExpression || {type: "Literal", value: undefined},
      {type: "Literal", value: aParentNode.range[0]},
      {type: "Literal", value: aParentNode.range[1]}
    ];
    if (aArgs) {
      for each (let arg in aArgs) {
        if (!arg.type) { // does not look like a node
          args.push({type: "Literal", value: arg});
        } else {         // pass AST node as-is as argument
          args.push(arg);
        }
      }
    }

    return {
      type: "CallExpression",
      callee: {type: "Identifier", name: aRecorder},
      arguments: args
    };
  },

  /*                      */
  /* ASTVisitor callbacks */

  onVariableDeclarator: function (aNode)
  {
    if (!aNode.init) {
      return; // ignore declared variable without initializer
    }
    aNode.init = this._wrapExpression(aNode.init,
                                      aNode,
                                      RecorderFunctionNames.VARIABLE_EVENT, [
                                        VariableEventType.DECLARATION,
                                        aNode.id.name
                                      ]);
  },

  onAssignmentExpression: function (aNode)
  {
    if (aNode.left.type != "Identifier") {
      return;
    }
    let wrapped = this._wrapExpression(aNode.operator == "=" ? aNode.right : aNode,
                                       aNode,
                                       RecorderFunctionNames.VARIABLE_EVENT, [
                                         VariableEventType.ASSIGNMENT,
                                         aNode.left.name
                                       ]);
    if (aNode.operator == "=") {
      aNode.right = wrapped
    } else {
      aNode.$replacement = wrapped;
    }
  },

  onUpdateExpression: function (aNode)
  {
    if (aNode.argument.type != "Identifier") {
      return;
    }
    aNode.$replacement = this._wrapExpression(aNode,
                                              aNode,
                                              RecorderFunctionNames.VARIABLE_EVENT, [
                                                VariableEventType.UPDATE,
                                                aNode.argument.name
                                              ]);
  },

  onReturnStatement: function (aNode)
  {
    aNode.argument = this._wrapExpression(aNode.argument,
                                          aNode,
                                          RecorderFunctionNames.RETURN_EVENT);
  },

  onForStatement: function (aNode)
  {
    if ((!aNode.test || aNode.test.type == "Identifier")
        || (!aNode.update || aNode.update.type == "Identifier")) {
      this._hasInfiniteLoop = true;
    }
  }
};
