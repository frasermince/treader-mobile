window.onerror = function (message, file, line, col, error) {
  var msg = JSON.stringify({method:"error", value: message});
  window.postMessage(msg, "*");
};
(function () {
function getWordRange(e) {
  // FF gives us a shortcut
  var target = e.explicitOriginalTarget || e.target,
    // We will use this to get the positions of our textNodes
    range = document.createRange(),
    rect, i;
  // so first let's get the textNode that was clicked
  if (target.nodeType !== 3) {
    var children = target.childNodes;
    var node = null;
    i = 0;
    while (i < children.length) {
      range.selectNode(children[i]);
      rect = range.getBoundingClientRect();
      if (rect.left <= e.pageX && rect.right >= e.pageX &&
        rect.top <= e.pageY && rect.bottom >= e.pageY) {
        target = children[i];
        if (target.nodeType !== 3 && target.tagName == "SPAN") {
          target = target.childNodes[0];
        }
        break;
      }
      i++;
    }
  }
  range.selectNode(target);
  console.log("***TARGET", target.tagName);
  if (target.nodeType !== 3) {
    return null;
  }
  console.log("***SHOULD HIT");

  var text = target.nodeValue || target.textContent;
  // Return range if span only has one word.
  console.log("TEXT", text);
  if (!text.includes(' ')) {
    range.setEnd(target, text.length);
    return range;
  }
  // Now, let's split its content to words
  var words = text.split(' '),
    textNode, newText;
  console.log("***WORDS", words)
  i = 0;
  while (i < words.length) {
    // create a new textNode with only this word
    textNode = document.createTextNode((i ? ' ' : '') + words[i]);
    newText = words.slice(i + 1);
    // update the original node's text
    target.nodeValue = newText.length ? (' ' + newText.join(' ')) : '';
    // insert our new textNode
    target.parentNode.insertBefore(textNode, target);
    // get its position
    range.selectNode(textNode);
    rect = range.getBoundingClientRect();
    // if it is the one
    if (rect.left <= e.clientX && rect.right >= e.clientX &&
      rect.top <= e.clientY && rect.bottom >= e.clientY) {
      console.log("RANGE", range.startOffset, range.endOffset);
      range.setEnd(textNode, newText.length);
      console.log("RANGE", range.startOffset, range.endOffset);
      return range;
    }
    i++;
  }
};
const findTitlesAndLanguage = (rendition, location) => {
  var sendMessage = function(obj) {
    // window.postMessage(JSON.stringify(obj), targetOrigin);
    if (!window.ReactNativeWebView.postMessage) {
      setTimeout(() => {
        sendMessage(obj);
      }, 1);
    } else {
      window.ReactNativeWebView.postMessage(JSON.stringify(obj));
    }
  };

  console.log = function() {
    sendMessage({method:"log", value: Array.from(arguments)});
  }
  console.error = function() {
    sendMessage({method:"error", value: Array.from(arguments)});
  }
  let book = rendition.book
  let spineItem = book.spine.get(location);
  let bookTitle = book.package.metadata.title
  console.log("***TITLE VARS");
  let language = book.package.metadata.language.toLowerCase();
  let chapterTitle = ""
  if (bookTitle == "Italian Riveduta 1927 (RIV)") {
    let chapterNumber = spineItem.href.slice(-7)[0]
    let titleHref = spineItem.href.slice(0, -7) + "0.xhtml"
    let secondDigit = spineItem.href.slice(-8)[0]
    if (secondDigit != ".") {
      titleHref = spineItem.href.slice(0, -8) + "0.xhtml"
      chapterNumber = secondDigit + chapterNumber;
    }
    let navItem = book.navigation.get(titleHref);
    chapterTitle = navItem.label.trim() + ` ${chapterNumber}`
  } else {
    let navItem = book.navigation.get(spineItem.href);
    if (navItem == null) {
      book.navigation.forEach((nav) => {
        if (nav.href.includes(spineItem.href)) {
          navItem = nav;
        }
      })
    }
    chapterTitle = navItem == null ? "" : navItem.label.trim()
  }
  return [chapterTitle, language];
}

function setTitlesAndLanguage(rendition, location) {
  var sendMessage = function(obj) {
    // window.postMessage(JSON.stringify(obj), targetOrigin);
    if (!window.ReactNativeWebView.postMessage) {
      setTimeout(() => {
        sendMessage(obj);
      }, 1);
    } else {
      window.ReactNativeWebView.postMessage(JSON.stringify(obj));
    }
  };
  const [chapterTitle, language] = findTitlesAndLanguage(rendition, location);
  sendMessage({method:"set", key: "chapterTitle", value: chapterTitle});
  sendMessage({method:"set", key: "language", value: language});
  return language;
}

function setWordInformation(highlightedContent, epubcfi, morphology) {
  var sendMessage = function(obj) {
    // window.postMessage(JSON.stringify(obj), targetOrigin);
    if (!window.ReactNativeWebView.postMessage) {
      setTimeout(() => {
        sendMessage(obj);
      }, 1);
    } else {
      window.ReactNativeWebView.postMessage(JSON.stringify(obj));
    }
  };

  console.log = function() {
    sendMessage({method:"log", value: Array.from(arguments)});
  }
  console.error = function() {
    sendMessage({method:"error", value: Array.from(arguments)});
  }
  console.log("***SET");
  console.log("***INFO", highlightedContent, epubcfi, morphology);
  sendMessage({method:"set", key: "highlightedContent", value: highlightedContent});
  sendMessage({method:"set", key: "epubcfi", value: epubcfi});
  sendMessage({method:"set", key: "morphology", jsonValue: JSON.stringify(morphology)});
}

function renditionHandler(rendition, location) {
  var sendMessage = function(obj) {
    // window.postMessage(JSON.stringify(obj), targetOrigin);
    if (!window.ReactNativeWebView.postMessage) {
      setTimeout(() => {
        sendMessage(obj);
      }, 1);
    } else {
      window.ReactNativeWebView.postMessage(JSON.stringify(obj));
    }
  };

  console.log = function() {
    sendMessage({method:"log", value: Array.from(arguments)});
  }
  console.error = function() {
    sendMessage({method:"error", value: Array.from(arguments)});
  }

  setTitlesAndLanguage(rendition, location);
  }
  function _ready() {
    var contents;
    var targetOrigin = "*";
    var sendMessage = function(obj) {
      // window.postMessage(JSON.stringify(obj), targetOrigin);
      if (!window.ReactNativeWebView.postMessage) {
        setTimeout(() => {
          sendMessage(obj);
        }, 1);
      } else {
        window.ReactNativeWebView.postMessage(JSON.stringify(obj));
      }
    };

    var q = [];
    var _isReady = false;

    var book;
    var rendition;

    var minSpreadWidth = 815;
    var axis = "horizontal";

    var isChrome = /Chrome/.test(navigator.userAgent);
    var isWebkit = !isChrome && /AppleWebKit/.test(navigator.userAgent);

    // debug
    console.log = function() {
      sendMessage({method:"log", value: Array.from(arguments)});
    }

    console.error = function() {
      sendMessage({method:"error", value: Array.from(arguments)});
    }

    function onMessage(e) {
      var message = e.data;
      handleMessage(message);
    }

    function handleMessage(message) {
      var decoded = (typeof message == "object") ? message : JSON.parse(message);
      var response;
      var result;

      switch (decoded.method) {
        case "open": {
          var url = decoded.args[0];
          var options = decoded.args.length > 1 && decoded.args[1];
          openEpub(url, options);

          if (options && options.webviewStylesheet) {
            var head = document.getElementsByTagName('head')[0];
            var link = document.createElement('link');
            link.rel = 'stylesheet';
            link.type = 'text/css';
            link.href = options.webviewStylesheet;
            head.appendChild(link);
          }

          if (options && options.script) {
            var head = document.getElementsByTagName('head')[0];
            var script = document.createElement('script');
            script.src = options.script
            head.appendChild(script);
          }
          break;
        }
        case "display": {
          var args = decoded.args && decoded.args.length && decoded.args[0];
          var target;

          if (!args) {
            target = undefined;
          }
          else if (args.target) {
            target = args.target.toString();
          }
          else if (args.spine) {
            target = parseInt(args.spine);
          }

          if (rendition) {
            rendition.display(target);
          } else {
            q.push(message);
          }
          break;
        }
        case "flow": {
          var direction = decoded.args.length && decoded.args[0];
          axis = (direction === "paginated") ? "horizontal" : "vertical";

          if (rendition) {
            rendition.flow(direction);
          } else {
            q.push(message);
          }

          break;
        }
        case "resize": {
          var width = decoded.args.length && decoded.args[0];
          var height = decoded.args.length > 1 && decoded.args[1];

          if (rendition) {
            rendition.resize(width, height);
          } else {
            q.push(message);
          }

          break;
        }
        case "setLocations": {
          var locations = decoded.args[0];
          if (book) {
            book.locations.load(locations);
          } else {
            q.push(message);
          }

          if (rendition) {
            rendition.reportLocation();
          }
          break;
        }
        case "reportLocation": {
          if (rendition) {
            rendition.reportLocation();
          } else {
            q.push(message);
          }
          break;
        }
        case "minSpreadWidth": {
          minSpreadWidth = decoded.args;
          break;
        }
        case "mark": {
          if (rendition) {
            rendition.annotations.mark.apply(rendition.annotations, decoded.args);
          } else {
            q.push(message);
          }
          break;
        }
        case "underline": {
          if (rendition) {
            rendition.annotations.underline.apply(rendition.annotations, decoded.args);
          } else {
            q.push(message);
          }
          break;
        }
        case "highlight": {
          if (rendition) {
            rendition.annotations.highlight.apply(rendition.annotations, decoded.args);
          } else {
            q.push(message);
          }
          break;
        }
        case "removeAnnotation": {
          if (rendition) {
            rendition.annotations.remove.apply(rendition.annotations, decoded.args);
          } else {
            q.push(message);
          }
          break;
        }
        case "themes": {
          var themes = decoded.args[0];
          if (rendition) {
            rendition.themes.register(themes);
          } else {
            q.push(message);
          }
          break;
        }
        case "theme": {
          var theme = decoded.args[0];
          if (rendition) {
            rendition.themes.select(theme);
          } else {
            q.push(message);
          }
          break;
        }
        case "fontSize": {
          var fontSize = decoded.args[0];
          if (rendition) {
            rendition.themes.fontSize(fontSize);
          } else {
            q.push(message);
          }
          break;
        }
        case "font": {
          var font = decoded.args[0];
          if (rendition) {
            rendition.themes.font(font);
          } else {
            q.push(message);
          }
          break;
        }
        case "override": {
          if (rendition) {
            rendition.themes.override.apply(rendition.themes, decoded.args);
          } else {
            q.push(message);
          }
          break;
        }
        case "gap": {
          var gap = decoded.args[0];
          if (rendition) {
            rendition.settings.gap = gap;
            if (rendition.manager) {
              rendition.manager.settings.gap = gap;
            }
          } else {
            q.push(message);
          }
          break;
        }
        case "next": {
          if (rendition) {
            rendition.next();
          } else {
            q.push(message);
          }
          break;
        }
        case "prev": {
          if (rendition) {
            rendition.prev();
          } else {
            q.push(message);
          }
          break;
        }
      }
    }

    function openEpub(url, options) {
      var settings = Object.assign({
        manager: "continuous",
        overflow: "hidden",
        method: "blobUrl",
        fullsize: true,
        snap: isChrome
      }, options);

      console.log("OPEN", url);
      window.book = book = ePub(url);

      console.log("AFTER EPUB");
      window.rendition = rendition = book.renderTo(document.body, settings);
      
      rendition.hooks.content.register(function(contents, rendition) {
        contents.triggerSelectedEvent = function(cfi){
          var range, cfirange;
          console.log("***SELECTED");
          if(cfi) {
            this.previouslySelected = true;
            // cfirange = this.section.cfiFromRange(range);
            this.emit("selected", cfi);
            //this.emit("selectedRange", range);
          } else if (this.previouslySelected) {
            console.log("***DESELECT");
            this.previouslySelected = false;
            this.emit("deselected");
          }
        }

        contents.on("deselected", function() {
          debugger;
          sendMessage({method:"set", key: "translation", value: null});
          setWordInformation(null, null, null);
        });

        var doc = contents.document;
        var startPosition = { x: -1, y: -1 };
        var currentPosition = { x: -1, y: -1 };
        var isLongPress = false;
        var longPressTimer;
        var touchduration = 300;
        var preventTap;
        var $body = doc.getElementsByTagName('body')[0];
        var previous = ""

        function touchStartHandler(e) {
          var f, target;

          startPosition.x = e.targetTouches[0].pageX;
          startPosition.y = e.targetTouches[0].pageY;
          currentPosition.x = e.targetTouches[0].pageX;
          currentPosition.y = e.targetTouches[0].pageY;
          isLongPress = false;

          if (isWebkit) {
            for (var i=0; i < e.targetTouches.length; i++) {
              f = e.changedTouches[i].force;
              if (f >= 0.8 && !preventTap) {
                target = e.changedTouches[i].target;

                if (target.getAttribute("ref") === "epubjs-mk") {
                  return;
                }

                clearTimeout(longPressTimer);

                cfi = contents.cfiFromNode(target).toString();

                sendMessage({method:"longpress", position: currentPosition, cfi: cfi});
                isLongPress = false;
                preventTap = true;
              }
            }
          }


          longPressTimer = setTimeout(function() {
            target = e.targetTouches[0].target;

            if (target.getAttribute("ref") === "epubjs-mk") {
              return;
            }

            cfi = contents.cfiFromNode(target).toString();

            sendMessage({method:"longpress", position: currentPosition, cfi: cfi});
            preventTap = true;
          }, touchduration);
        }

        function touchMoveHandler(e) {
          currentPosition.x = e.targetTouches[0].pageX;
          currentPosition.y = e.targetTouches[0].pageY;
          clearTimeout(longPressTimer);
        }

        function touchEndHandler(e) {
          var txt = document.getSelection().toString();
            console.log("***SELECTED", txt);
          if (txt || previous && !(txt && previous)) {
            console.log("***SEND EVENT");
            window.ReactNativeSelectDetection.postMessage(txt);
            previous = txt;
          }

          var cfi;
          clearTimeout(longPressTimer);

          if(preventTap) {
            preventTap = false;
            return;
          }

          if(Math.abs(startPosition.x - currentPosition.x) < 2 &&
             Math.abs(startPosition.y - currentPosition.y) < 2) {

            var target = e.changedTouches[0].target;

            if (target.getAttribute("ref") === "epubjs-mk" ||
                target.getAttribute("ref") === "epubjs-hl" ||
                target.getAttribute("ref") === "epubjs-ul") {
              return;
            }
                        //var elem = e.target;
            //var selRange = elem.createTextRange();
            //var start = 0;
            //var end = elem.value.length;
            //selRange.collapse(true);
            //selRange.moveStart('character', start);
            //selRange.moveEnd('character', end);
            //selRange.select();

            var range = getWordRange(e)
            if (range) {
              cfi = contents.cfiFromRange(range);
              contents.triggerSelectedEvent(cfi);
            } else {
              contents.triggerSelectedEvent(null);
              cfi = contents.cfiFromNode(target).toString();
            }

            if(isLongPress) {
              sendMessage({method:"longpress", position: currentPosition, cfi: cfi});
              isLongPress = false;
            } else {
              setTimeout(function() {
                if(preventTap) {
                  preventTap = false;
                  isLongPress = false;
                  return;
                }
                sendMessage({method:"press", position: currentPosition, cfi: cfi});
              }, 10);
            }
          }
        }

        function touchForceHandler(e) {
          var f = e.changedTouches[0].force;
          if (f >= 0.8 && !preventTap) {
            var target = e.changedTouches[0].target;

            if (target.getAttribute("ref") === "epubjs-mk") {
              return;
            }

            clearTimeout(longPressTimer);

            cfi = contents.cfiFromNode(target).toString();

            sendMessage({method:"longpress", position: currentPosition, cfi: cfi});
            isLongPress = false;
            preventTap = true;
          }
        }

        doc.addEventListener("touchstart", touchStartHandler, false);

        doc.addEventListener("touchmove", touchMoveHandler, false);

        doc.addEventListener("touchend", touchEndHandler, false);

        doc.addEventListener('touchforcechange', touchForceHandler, false);

      }.bind(this));

      //console.log("***RENDITION", rendition.);

      rendition.on("relocated", function(location){
        console.log("RELOCATED");
        //renditionHandler(rendition, options.location);
        const [chapterTitle, language] = findTitlesAndLanguage(rendition, options.location);
        sendMessage({method:"set", key: "chapterTitle", value: chapterTitle});
        sendMessage({method:"relocated", location: location});
      });

      rendition.on("selected", function(cfiRange) {
        this.book.getRange(cfiRange).then((range) => {
          let span = range.commonAncestorContainer.parentElement; //TODO
          let text = range.toString();
          if (span && span.tagName.toLowerCase() == 'span' ) {
            console.log("HIGHLIGHTED", text);
            setWordInformation(text, cfiRange, span.dataset);
          } else {
            console.log("HIGHLIGHTED MULTI", text);
            setWordInformation(text, cfiRange, null);
          }
        });
      });

      rendition.on("markClicked", function (cfiRange, data) {
        sendMessage({method:"markClicked", cfiRange: cfiRange, data: data});
      });

      rendition.on("rendered", function (section) {
        console.log("RENDERED");
        renditionHandler(rendition, options.location);
        sendMessage({method:"rendered", sectionIndex: section.index});
      });

      rendition.on("added", function (section) {
        sendMessage({method:"added", sectionIndex: section.index});
      });

      rendition.on("removed", function (section) {
        sendMessage({method:"removed", sectionIndex: section.index});
      });

      rendition.on("resized", function(size){
        sendMessage({method:"resized", size: size});
      });

      // replay messages
      rendition.started.then(function() {
        var msg;
        for (var i = 0; i < q.length; i++) {
          msg = q.shift();
          handleMessage(msg);
        }
      });

      book.ready.then(function(){
        _isReady = true;

        sendMessage({method:"ready"});

      });

      window.addEventListener("unload", function () {
        book && book.destroy();
      });
    }

    window.addEventListener("message", onMessage);
    // React native uses document for postMessages
    document.addEventListener("message", onMessage);

    sendMessage({method:"loaded", value: true});
  }

  if ( document.readyState === 'complete' ) {
    console.log("***READY");
    _ready();
  } else {
    window.addEventListener("load", _ready, false);
  }
}());

// Object.assign polyfill -- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/assign
if (typeof Object.assign !== 'function') {
  // Must be writable: true, enumerable: false, configurable: true
  Object.defineProperty(Object, "assign", {
    value: function assign(target, varArgs) { // .length of function is 2
      'use strict';
      if (target === null || target === undefined) {
        throw new TypeError('Cannot convert undefined or null to object');
      }

      var to = Object(target);

      for (var index = 1; index < arguments.length; index++) {
        var nextSource = arguments[index];

        if (nextSource !== null && nextSource !== undefined) {
          for (var nextKey in nextSource) {
            // Avoid bugs when hasOwnProperty is shadowed
            if (Object.prototype.hasOwnProperty.call(nextSource, nextKey)) {
              to[nextKey] = nextSource[nextKey];
            }
          }
        }
      }
      return to;
    },
    writable: true,
    configurable: true
  });
}
