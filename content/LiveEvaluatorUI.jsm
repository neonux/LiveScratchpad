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

const Cu = Components.utils;
Cu.import("resource://gre/modules/Services.jsm");

const HTML_NS = "http://www.w3.org/1999/xhtml";
const QUOTED_STRING_RE = /^["'].*["']$/;

const DEADCODE_ANNOTATION = "scratchpad.deadcode";
const LIVE_FUNCTION_ANNOTATION = "scratchpad.livefunction";

/**
 * LiveEvaluatorUI constructor.
 *
 * This provides UI to represent the attached LiveEvaluator's events to a human.
 * Technicially, this is an implementation of ILiveEvaluatorObserver.
 *
 * @param DOMWindow aOwnerWindow
 * @param DOMElement aRoot
 * @see evaluator
 * @see LiveEvaluator.addObserver
 */
function LiveEvaluatorUI(aOwnerWindow, aRoot)
{
  this._ownerWindow = aOwnerWindow;
  this._root = aRoot;
  this._evaluator = null;

  this._document = aRoot.ownerDocument;
  this._funcArgumentsList = this._root.querySelector(".function-arguments");
  this._funcEventsList = this._root.querySelector(".function-events");
  this._parseErrorItem = this._root.querySelector("li[data-abort-reason=PARSE_ERROR]");

  this._onArgumentClickBinding = this._onArgumentClick.bind(this);
  this._onArgumentInputBinding = this._onArgumentInput.bind(this);
  this._onArgumentKeyDownBinding = this._onArgumentKeyDown.bind(this);
  this._onMouseOverBinding = this._onMouseOver.bind(this);

  this._representers = [];
  this.registerRepresenter(new PrimitiveRepresenter(this));
  this.registerRepresenter(new ObjectRepresenter(this));
  this.registerRepresenter(new DOMElementRepresenter(this));

  this._setup();
}

LiveEvaluatorUI.prototype =
{
  /**
   * Retrieve the owner window of the UI.
   */
  get ownerWindow() this._ownerWindow,

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
   * Register a representer.
   *
   * Representer objects are responsible for generating the visual presentation
   * of an evaluation event.
   * Events are attempted to be presented by representers in reverse order of
   * registration (eg. the latest registered representer can override presentation
   * of any value if desired).
   *
   * ILiveEvaluatorUIRepresenter {
   *   // request to represent the given value into the given container.
   *   // returns true if the value has been presented, false otherwise.
   *   boolean representValue(any aValue, DOMElement aValueContainer)
   * }
   *
   * @param ILiveEvaluatorUIRepresenter aRepresenter
   */
  registerRepresenter: function LEUI_registerRepresenter(aRepresenter)
  {
    this._representers.push(aRepresenter);
  },

  /**
   * Unregister a representer.
   *
   * @param ILiveEvaluatorUIRepresenter aRepresenter
   */
  unregisterRepresenter: function LEUI_unregisterRepresenter(aRepresenter)
  {
    let index = this._representers.indexOf(aRepresenter);
    if (index != -1) {
      this._representers.splice(index, 1);
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
      node.querySelector("dd").focus();
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
    el.className = this._getTokenStyle(el.textContent);

    this.evaluator.setArgument(el.parentNode.parentNode.dataset.argumentIndex,
                               el.textContent);
    this.evaluator.evaluate();
  },

  /**
   * Called when a key has been pressed in an argument input element.
   *
   * @param DOMEvent aEvent
   */
  _onArgumentKeyDown: function LEUI__onArgumentKeyDown(aEvent)
  {
    switch (aEvent.keyCode) {
    case aEvent.DOM_VK_ENTER:
    case aEvent.DOM_VK_RETURN:
      aEvent.target.blur();
      aEvent.preventDefault();
      break;
    }

    // number adjustment key bindings
    if (aEvent.altKey) {
      let target = aEvent.target;
      let value = parseFloat(target.textContent);
      if (!isNaN(value)) {
        switch (aEvent.keyCode) {
        case aEvent.DOM_VK_UP:
          value += 1;
          break;
        case aEvent.DOM_VK_DOWN:
          value -= 1;
          break;
        case aEvent.DOM_VK_PAGE_UP:
          value *= 2;
          break;
        case aEvent.DOM_VK_PAGE_DOWN:
          value /= 2;
          break;
        }
        target.textContent = value.toString();
        this._onArgumentInput(aEvent);
      }
    }
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

    let valueContainer = null;
    if (aName) {
      let container = this._document.createElementNS(HTML_NS, "div");
      let nameContainer = this._document.createElementNS(HTML_NS, "dt");
      nameContainer.textContent = aName;

      valueContainer = this._document.createElementNS(HTML_NS, "dd");

      container.appendChild(nameContainer);
      container.appendChild(valueContainer);
      item.appendChild(container);
    }
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
    item.addEventListener("keydown", this._onArgumentKeyDownBinding, false);

    valueContainer.setAttribute("contenteditable", "");
    valueContainer.textContent = "undefined";
    valueContainer.className = this._getTokenStyle(valueContainer.textContent);
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
    for (var i = this._representers.length - 1; i >= 0; i--) {
      if (this._representers[i].representValue(aValue, valueContainer)) {
        break; // we got a presentation!
      }
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

  onEditorDetach: function LEO_onEditorDetach(aEvaluator)
  {
    if (this._annotationAdded) {
      let annotationModel = aEvaluator.editor._annotationModel;
      annotationModel.removeAnnotations(DEADCODE_ANNOTATION);
      annotationModel.removeAnnotations(LIVE_FUNCTION_ANNOTATION);
    }
  },

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
    // annotate dead code
    // FIXME: expose necessary SourceEditor API, do proper branch detection in
    //        with additional necessary probes in the evaluator
    if (!aEvaluator.editor._annotationStyler) {
      return; // Fx<14 compatibility
    } else if (!this._annotationAdded) {
      aEvaluator.editor._annotationStyler.addAnnotationType(DEADCODE_ANNOTATION);
      aEvaluator.editor._annotationStyler.addAnnotationType(LIVE_FUNCTION_ANNOTATION);
      this._annotationAdded = true;
    }

    let annotationModel = aEvaluator.editor._annotationModel;
    annotationModel.removeAnnotations(DEADCODE_ANNOTATION);
    annotationModel.removeAnnotations(LIVE_FUNCTION_ANNOTATION);

    annotationModel.addAnnotation({
      start: aNode.range[0],
      end: aNode.range[0] + "function".length,
      type: LIVE_FUNCTION_ANNOTATION,
      rangeStyle: {styleClass: "token_keyword", style: {fontWeight: "bold"}},
    });

    for (let branchRange in aEvaluator.branches) {
      if (!aEvaluator.branches[branchRange]) {
        branchRange = branchRange.split("-");
        annotationModel.addAnnotation({
          start: parseInt(branchRange[0]),
          end: parseInt(branchRange[1]),
          type: DEADCODE_ANNOTATION,
          lineStyle: {style: {opacity: "0.5", fontStyle: "oblique"}}
        });
      }
    }
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

  onBranchEvent: function LEO_onBranchEvent(aEvaluator, aRangeStart, aRangeEnd, aEventType)
  {
    if (aEventType == "iteration") {
      let [item, valueContainer] = this._createItem(null, aRangeStart, aRangeEnd);
      item.className = "loop " + aEventType;
      this._funcEventsList.appendChild(item);
    }
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

/*                       */
/* built-in representers */

/**
 * AbstractRepresenter provides helper methods for Representer objects.
 */
var AbstractRepresenter =
{
  /**
   * Retrieve the parent item node for the given value container.
   *
   * @param DOMElement aValueContainer
   */
  getItemNode: function AR_getItemNode(aValueContainer)
  {
    return aValueContainer.parentNode.parentNode;
  },

  /**
   * Add a new presentation container of a given name into the given item.
   *
   * @param string aName
   *        The name of the presentation. This will be set as the class name
   *        of the presentation container.
   * @param DOMElement aItem
   */
  addPresentationContainer: function AR_addPresentationContainer(aName, aItem)
  {
    let container = aItem.ownerDocument.createElementNS(HTML_NS, "dd");
    container.className = aName;
    aItem.firstElementChild.appendChild(container);
    return container;
  },

  /**
   * Checks if the presentation container of given name exists inside given item.
   *
   * @param string aName
   * @param DOMElement aItem
   */
  hasPresentationContainer: function AR_hasPresentationContainer(aName, aItem)
  {
    return !!aItem.querySelector("dd." + aName);
  }
};

/**
 * PrimitiveRepresenter constructor.
 * This representer displays primitive JS values (ie. not objects)
 */
function PrimitiveRepresenter()
{
}

PrimitiveRepresenter.prototype =
{
  representValue: function PR_representValue(aValue, aValueContainer)
  {
    switch (typeof(aValue)) {
    case "undefined":
    case "boolean":
      aValueContainer.classList.add("token_keyword");
      aValueContainer.textContent = aValue === undefined
                                   ? "undefined" : aValue.toString();
      return true;
    case "object":
      if (aValue === null) {
        aValueContainer.classList.add("token_keyword");
        aValueContainer.textContent = "null";
        return true;
      }
      break;
    case "number":
      aValueContainer.classList.add(isNaN(aValue)
                                   ? "token_keyword" : "token_number");
      aValueContainer.textContent = aValue.toString();
      return true;
    case "string":
      aValueContainer.classList.add("token_string");
      aValueContainer.textContent = '"' + aValue + '"';
      return true;
    case "function":
      aValueContainer.classList.add("token_keyword");
      aValueContainer.textContent = "function";
      return true;
    }
    return false;
  }
};

/**
 * ObjectRepresenter constructor.
 * This representer displays JS object values lazily and recursively (on click).
 */
function ObjectRepresenter(aUI)
{
  this._UI = aUI;
  this._onClickBinding = this._onClick.bind(this);
}

ObjectRepresenter.prototype =
{
  USER_DATA_OBJECT: "scratchpad:object",

  representValue: function OR_representValue(aValue, aValueContainer)
  {
    if (!aValue || typeof(aValue) != "object") {
      return false;
    }

    let item = AbstractRepresenter.getItemNode(aValueContainer);
    item.setUserData(this.USER_DATA_OBJECT, aValue, null);

    aValueContainer.classList.add("actionable");
    if (Object.prototype.toString.call(aValue) == "[object Array]") {
      aValueContainer.textContent = "[" + (aValue.length ? "..." : "") + "]";
    } else {
      aValueContainer.textContent = aValue.toString();
    }
    aValueContainer.addEventListener("click", this._onClickBinding, false);
    return true;
  },

  _onClick: function OR__onClick(aEvent)
  {
    let valueContainer = aEvent.currentTarget;
    let item = AbstractRepresenter.getItemNode(valueContainer);
    if (!AbstractRepresenter.hasPresentationContainer("full", item)) {
      this._createFullRepresentation(item);
    } else {
      valueContainer.classList.toggle("collapsed");
    }
  },

  _createFullRepresentation: function OR__createFullRepresentation(aItem)
  {
    let container = AbstractRepresenter.addPresentationContainer("full", aItem);
    let obj = aItem.getUserData(this.USER_DATA_OBJECT);
    let list = aItem.ownerDocument.createElementNS(HTML_NS, "ol");
    for (var key in obj) {
      try {
        list.appendChild(this._UI._createValueItem(key, obj[key]));
      } catch (ex) {
        /* swallow exceptions due to faulty getters */
      }
    }
    if (list.children.length) {
      container.appendChild(list);
    } else {
      container.textContent = "{}";
    }
  },
};

/**
 * DOMElementRepresenter constructor.
 * This representer displays DOM elements values, click to inspect element.
 */
function DOMElementRepresenter(aUI)
{
  this._UI = aUI;
  this._onClickBinding = this._onClick.bind(this);
}

DOMElementRepresenter.prototype =
{
  USER_DATA_ELEMENT: "scratchpad:element",

  get inspectorUI()
  {
    let browserWindow = Services.wm.getMostRecentWindow("navigator:browser");
    return browserWindow ? browserWindow.InspectorUI : null;
  },

  representValue: function DER_representValue(aValue, aValueContainer)
  {
    if (!(aValue instanceof Components.interfaces.nsIDOMElement)) {
      return false;
    }

    let item = AbstractRepresenter.getItemNode(aValueContainer);
    item.setUserData(this.USER_DATA_ELEMENT, aValue, null);

    aValueContainer.classList.add("actionable");
    aValueContainer.classList.add("element");
    let elementRepr = aValue.nodeName.toLowerCase();
    if (aValue.id) {
      elementRepr += "#" + aValue.id;
    }
    if (aValue.classList.length) {
      for (var i = 0; i < aValue.classList.length; ++i) {
        elementRepr += "." + aValue.classList[i];
      }
    }
    aValueContainer.textContent = elementRepr;
    aValueContainer.addEventListener("click", this._onClickBinding, false);
    return true;
  },

  _onClick: function OR__onClick(aEvent)
  {
    let valueContainer = aEvent.currentTarget;
    let item = AbstractRepresenter.getItemNode(valueContainer);
    let el = item.getUserData(this.USER_DATA_ELEMENT);
    if (this.inspectorUI) {
      if (!this.inspectorUI.isInspectorOpen) {
        this.inspectorUI.openInspectorUI(el);
      } else {
        if (this.inspectorUI.selection != el) {
          this.inspectorUI.select(el, true, true, "scratchpad");
        } else {
          this.inspectorUI.closeInspectorUI();
        }
      }

      //FIXME: workaround inspector forcing focus to the browser window!
      //       this should be the caller responsibility to focus it if desired
      this._UI.ownerWindow.setTimeout(function () {
        this._UI.ownerWindow.focus();
      }.bind(this), 0);
    }
  }
};
