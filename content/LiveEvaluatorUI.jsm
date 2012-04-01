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

const EXPORTED_SYMBOLS = ["LiveEvaluatorUI"];

const HTML_NS = "http://www.w3.org/1999/xhtml";
const QUOTED_STRING_RE = /^["'].*["']$/;

const USER_DATA_ELEMENT = "scratchpad:element";

/**
 * LiveEvaluatorUI constructor.
 *
 * This provides UI to represent the attached LiveEvaluator's events to a human.
 * Technicially, this is an implementation of ILiveEvaluatorObserver.
 *
 * @param DOMElement aRoot
 * @see evaluator
 * @see LiveEvaluator.addObserver
 */
function LiveEvaluatorUI(aRoot)
{
  this._root = aRoot;
  this._evaluator = null;

  this._document = aRoot.ownerDocument;
  this._funcArgumentsList = this._root.querySelector(".function-arguments");
  this._funcEventsList = this._root.querySelector(".function-events");
  this._parseErrorItem = this._root.querySelector("li[data-abort-reason=PARSE_ERROR]");

  this._onArgumentClickBinding = this._onArgumentClick.bind(this);
  this._onArgumentInputBinding = this._onArgumentInput.bind(this);
  this._onMouseOverBinding = this._onMouseOver.bind(this);

  this._setup();
}

LiveEvaluatorUI.prototype =
{
  /**
   * Retrieve the attached evaluator.
   * @return LiveEvaluator
   */
  get evaluator() this._evaluator,

  /**
   * Set the evaluator to attach to this UI.
   *
   * @param {LiveEvaluator} aEvaluator
   */
  set evaluator(aEvaluator)
  {
    if (this._evaluator) {
      this._evaluator.removeObserver(this);
    }
    this._evaluator = aEvaluator;
    if (this._evaluator) {
      this._evaluator.addObserver(this);
    }
  },

  /**
   * Set up the UI.
   */
  _setup: function LEUI__setup()
  {
    this._funcArgumentsList.addEventListener("click",
                                             this._onArgumentClickBinding, false);
    this._parseErrorItem.addEventListener("mouseover",
                                          this._onMouseOverBinding, false);
  },

  /**
   * Called when an argument list item is clicked.
   *
   * @param DOMMouseEvent aEvent
   */
  _onArgumentClick: function LEUI__onArgumentClick(aEvent)
  {
    let node = aEvent.target;
    while (node && node.tagName != "LI") {
      node = node.parentNode;
    }
    if (node) {
      node.querySelector(".value").focus();
    }
  },

  /**
   * Called when an argument value has been edited.
   *
   * @param DOMEvent aEvent
   */
  _onArgumentInput: function LEUI__onArgumentInput(aEvent)
  {
    let el = aEvent.target;
    el.className = "value " + this._getTokenStyle(el.textContent);

    this.evaluator.setArgument(el.parentNode.parentNode.dataset.argumentIndex,
                               el.textContent);
    this.evaluator.evaluate();
  },

  /**
   * Called when the cursor enters over a list item.
   *
   * @param DOMMouseEvent aEvent
   */
  _onMouseOver: function LEUI__onMouseOver(aEvent)
  {
    let target = aEvent.currentTarget;

    // source code highlighting
    if (target.dataset.rangeStart >= 0) {
      this.evaluator.editor.setSelection(parseInt(target.dataset.rangeStart),
                                         parseInt(target.dataset.rangeEnd) + 1);
    }
  },

  /**
   * Create a new list item.
   *
   * @param string aName
   * @param {number} aRangeStart
   * @param {number} aRangeEnd
   * @return [DOMElement item, DOMElement valueContainer]
   */
  _createItem: function LEUI__createItem(aName, aRangeStart, aRangeEnd)
  {
    let item = this._document.createElementNS(HTML_NS, "li");
    item.dataset.rangeStart = aRangeStart;
    item.dataset.rangeEnd = aRangeEnd;
    item.addEventListener("mouseover", this._onMouseOverBinding, false);

    let container = this._document.createElementNS(HTML_NS, "div");
    let nameContainer = this._document.createElementNS(HTML_NS, "dt");
    nameContainer.textContent = aName;

    let valueContainer = this._document.createElementNS(HTML_NS, "dd");

    container.appendChild(nameContainer);
    container.appendChild(valueContainer);
    item.appendChild(container);
    return [item, valueContainer];
  },

  /**
   * Create a new list item for an argument.
   *
   * @param {number} aIndex
   *        The zero-based index of the argument in the function argument list.
   * @param object aNode
   *        The AST node for the argument.
   * @return DOMElement
   */
  _createArgumentItem: function LEUI__createArgumentItem(aIndex, aNode)
  {
    let [item, valueContainer] = this._createItem(aNode.name, 
                                                  aNode.range[0], aNode.range[1]);
    item.dataset.argumentIndex = aIndex;
    item.addEventListener("input", this._onArgumentInputBinding, false);
    item.addEventListener("keydown", function onArgumentKeyDown(aEvent) {
      switch (aEvent.keyCode) {
      case aEvent.DOM_VK_ENTER:
      case aEvent.DOM_VK_RETURN:
        aEvent.target.blur();
        aEvent.preventDefault();
        break;
      }
    }, false);

    valueContainer.setAttribute("contenteditable", "");
    valueContainer.textContent = "undefined";
    valueContainer.className = "value "
                               + this._getTokenStyle(valueContainer.textContent);
    return item;
  },

  /**
   * Create a new list item for a value.
   *
   * @param string aName
   * @param any aValue
   * @param {number} aRangeStart
   * @param {number} aRangeEnd
   * @return DOMElement
   */
  _createValueItem: function LEUI__createValueItem(aName, aValue, aRangeStart, aRangeEnd)
  {
    let [item, valueContainer] = this._createItem(aName, aRangeStart, aRangeEnd);

    //TODO: extract to Representer objects >> creates the item?
    //      bool representer.apply(item, valueContainer)
    //           click/hover should be handled by the Representer
    //      + add a registerVisualizer API to allow add-ons with custom repr
    //      LiveEvaluatorUI.registerRepresenter(representer);
    switch (typeof(aValue)) {
    case "undefined":
    case "boolean":
      valueContainer.classList.add("token_keyword");
      valueContainer.textContent = aValue === undefined
                                   ? "undefined" : aValue.toString();
      break;
    case "object":
      if (aValue === null) {
        valueContainer.classList.add("token_keyword");
        valueContainer.textContent = "null";
      } else {
        if (aValue instanceof Components.interfaces.nsIDOMElement) {
          item.setUserData(USER_DATA_ELEMENT, aValue, null);
          valueContainer.classList.add("element");
          let elementRepr = aValue.nodeName.toLowerCase();
          if (aValue.id) {
            elementRepr += "#" + aValue.id;
          }
          if (aValue.classList.length) {
            for (var i = 0; i < aValue.classList.length; ++i) {
              elementRepr += "." + aValue.classList[i];
            }
          }
          valueContainer.textContent = elementRepr;
        } else {
          //TODO: collapsed JSON => open panel?
          valueContainer.textContent = aValue.toString();
        }
      }

      break;
    case "number":
      valueContainer.classList.add(isNaN(aValue)
                                   ? "token_keyword" : "token_number");
      valueContainer.textContent = aValue.toString();
      break;
    case "string":
      valueContainer.classList.add("token_string");
      valueContainer.textContent = '"' + aValue + '"';
      break;
    }
    return item;
  },

  /**
   * Retrieve the appropriate SourceEditor style class name for a given token.
   * If no appropriate style is found, an empty string is returned.
   *
   * @return string
   */
  _getTokenStyle: function LEUI__getTokenStyle(aToken)
  {
    switch (aToken) {
    case "undefined":
    case "null":
    case "true":
    case "false":
    case "NaN":
      return "token_keyword";
    default:
      if (QUOTED_STRING_RE.test(aToken)) {
        return "token_string";
      } else if (!isNaN(parseFloat(aToken))) {
        return "token_number";
      }
      break;
    }
    return "";
  },

  /**
   * Retrieve the start offset and the end offset of a given line.
   *
   * @param number aLineNumber
   *        Zero-based line number.
   * @return [aRangeStart, aRangeEnd];
   */
  _getLineRange: function LEUI__getLineRange(aLineNumber)
  {
    if (!this.evaluator.editor.getLineStart) { // Fx<14
      return [-1, -1];
    }
    return [this.evaluator.editor.getLineStart(aLineNumber),
            this.evaluator.editor.getLineEnd(aLineNumber)];
  },

  /*                                       */
  /* ILiveEvaluatorObserver implementation */
  /* @see LiveEvaluator.addObserver        */

  onStartEvaluation: function LEO_onStartEvaluation(aEvaluator, aNode)
  {
    delete this._root.dataset.abortReason;

    if (aNode.id.name != this._lastEvaluatedFunctionName
        || aNode.params.length != this._lastEvaluatedFunctionArgumentCount) {
      // re-generate argument list for the function to evaluate
      this._lastEvaluatedFunctionName = aNode.id.name;
      this._lastEvaluatedFunctionArgumentCount = aNode.params.length;
      this._funcArgumentsList.innerHTML = "";
      this.evaluator.resetArguments();
      for (let i = 0; i < aNode.params.length; ++i) {
        let arg = aNode.params[i];
        this._funcArgumentsList.appendChild(this._createArgumentItem(i, arg));
        this.evaluator.setArgument(i, "undefined");
      }
    }
    this._funcEventsList.innerHTML = "";
  },

  onStopEvaluation: function LEO_onStopEvaluation(aEvaluator, aNode)
  {
    //TODO: calculate dead code sections here, gray them out
    //      all within conditional not entered (need to instrument iteration,
    //      consequent and alternate (add block if none?).
    //      if tests => green|red (need to instrument tests)
    //      clear all deadcode annotations in whole editor before evaluation
  },

  onAbortEvaluation: function LEO_onAbortEvaluation(aEvaluator, aReason, aError)
  {
    //FIXME: dependency on Reason enum, better reporting, show error location in editor
    this._root.dataset.abortReason = aReason;
    if (aReason == "PARSE_ERROR") {
      this._parseErrorItem.querySelector("dd").textContent = aError.message;
      let [rangeStart, rangeEnd] = this._getLineRange(aError.lineNumber - 1);
      this._parseErrorItem.dataset.rangeStart = rangeStart;
      this._parseErrorItem.dataset.rangeEnd = rangeEnd - 1;
    }
  },

  onVariableEvent: function LEO_onVariableEvent(aEvaluator, aValue, aRangeStart, aRangeEnd, aEventType, aName)
  {
    let item = this._createValueItem(aName, aValue, aRangeStart, aRangeEnd);
    item.classList.add(aEventType);
    this._funcEventsList.appendChild(item);
  },

  onReturnEvent: function LEO_onReturnEvent(aEvaluator, aValue, aRangeStart, aRangeEnd)
  {
    let item = this._createValueItem("return", aValue, aRangeStart, aRangeEnd);
    item.classList.add("return");
    item.querySelector("dt").classList.add("token_keyword");
    this._funcEventsList.appendChild(item);
  },

  onUnhandledException: function LEO_onUnhandledException(aEvaluator, aException)
  {
    //TODO: ExceptionRepresenter that toggles stack trace display
    let ln = aException.lineNumber || -1;
    let [rangeStart, rangeEnd] = this._getLineRange(ln - 1);
    let item = this._createValueItem(aException.name, aException.message,
                                     rangeStart, rangeEnd);
    item.classList.add("exception");
    this._funcEventsList.appendChild(item);
  }
};
