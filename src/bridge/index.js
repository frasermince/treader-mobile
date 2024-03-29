import ePub from 'epubjs'
import tokenizer from 'sbd';
(function () {
  var messages = {};
  function sendMessageWithoutCache(obj) {
    if (!window.ReactNativeWebView.postMessage) {
      setTimeout(() => {
        sendMessageWithoutCache(obj);
      }, 1);
    } else {
      window.ReactNativeWebView.postMessage(JSON.stringify(obj));
    }
  }

  console.log = function() {
    sendMessageWithoutCache({method:"log", value: Array.from(arguments)});
  }
  console.error = function() {
    sendMessageWithoutCache({method:"error", value: Array.from(arguments)});
  }

  window.onerror = function (message, file, line, col, error) {
    console.error(message);
  };

  function sendMessage(obj) {
    // window.postMessage(JSON.stringify(obj), targetOrigin);
    var key = obj["key"] || obj["method"]
    var methodInCache = JSON.stringify(messages[key]) == JSON.stringify(obj)
    if (!methodInCache) {
      messages[key] = obj;
      sendMessageWithoutCache(obj);
    }
  };

  function getNextWord(range, isPrevious) {
    if (isPrevious && range.startContainer.tagName == "SPAN" && range.startContainer.previousElementSibling) {
      let nextRange = range.startContainer.previousElementSibling;
      range.setStart(nextRange, 0);
    } else if (range.endContainer.parentElement.tagName == "SPAN" && range.endContainer.parentElement.nextElementSibling) {
      let nextRange = range.endContainer.parentElement.nextElementSibling.childNodes[0];
      range.setEnd(nextRange, nextRange.textContent.length);
    }
    return range
  }

  function ltrim(str) {
    if(!str) return str;
    str = str.replace(/(\r\n|\n|\r)/gm, "");
    str = str.replace(/\s\s+/g, ' ');
    str = str.replace(/^\s+/g, '');
    return str.replace(/^—/g, '');
  }

  function getSentence(range) {
    let wordLength = range.endOffset;
    let paragraphRange = new Range();
    let sentenceOffset = 0;
    let phraseOffset = 0;
    paragraphRange.setStart(range.startContainer.closest("p"), 0);
    paragraphRange.setEnd(range.startContainer, 0);
    let paragraphText = range.commonAncestorContainer.parentElement.textContent.trim().replace(/(\r\n|\n|\r)/gm, "");
    let sentences = Array.from(range.startContainer.closest("p").getElementsByClassName("sentence")).map((e) => e.textContent);
    let sentenceSpan = true
    if (sentences.length == 0) {
      sentences = tokenizer.sentences(paragraphText, {"sanitize": true});
      sentenceSpan = false
    }
    let sentenceCharacters = ltrim(paragraphRange.toString()).length;
    let characterIteration = 0;
    let firstSentence = true;
    let sentence = sentences.find((sentence) => {
      // + 1 for the space that has been removed
      if (firstSentence) {
        sentence = ltrim(sentence);
        firstSentence = false
      }
      let total = characterIteration + sentence.length;
      if (total >= sentenceCharacters) {
        sentenceOffset = sentenceCharacters - characterIteration;
        return true;
      } else {
        characterIteration = sentenceSpan ? total : total + 1;
      }
    });
    let phrases = sentence.split(/(,|;|–)/g);
    let phrase = phrases.find((phrase) => {
      if (characterIteration + phrase.length > sentenceCharacters) {
        phraseOffset = sentenceCharacters - characterIteration;
        return true;
      } else {
        characterIteration += phrase.length;
      }
    });

    let multiRange = getNextWord(range.cloneRange(), true);
    multiRange = getNextWord(multiRange, true);
    multiRange = getNextWord(multiRange, false);
    multiRange = getNextWord(multiRange, false);
    let surrounding = ltrim(multiRange.toString());
    let sentenceWhitespaceLength = sentence.length - ltrim(sentence).length
    let phraseWhitespaceLength = phrase.length - ltrim(phrase).length
    sentence = sentence.trim().replace(/(\r\n|\n|\r)/gm, "").replace(/\s\s+/g, ' ');
    phrase = phrase.trim().replace(/(\r\n|\n|\r)/gm, "").replace(/\s\s+/g, ' ');
    sentenceOffset -= sentenceWhitespaceLength;
    phraseOffset -= phraseWhitespaceLength;
    return {sentence, phrase, surrounding, sentenceOffset, phraseOffset, wordLength};
  }

  function getWordRange(e) {
    // FF gives us a shortcut
    var target = e.explicitOriginalTarget || e.target,
      // We will use this to get the positions of our textNodes
      range = document.createRange(),
      rect, i;
    // so first let's get the textNode that was clicked
    if (target.nodeType !== 3) {
      var children = target.getElementsByClassName("word");
      children = children.length == 0 ? target.getElementsByTagName("span") : children;
      children = children.length == 0 ? target.childNodes : children;
      var node = null;
      i = 0;
      while (i < children.length) {
        range.selectNode(children[i]);
        rect = range.getBoundingClientRect();
        if (rect.left - 2 <= e.changedTouches[0].pageX && rect.right + 2 >= e.changedTouches[0].pageX &&
          rect.top - 6 <= e.changedTouches[0].pageY && rect.bottom + 4 >= e.changedTouches[0].pageY) {
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
    if (target.nodeType !== 3) {
      return null;
    }

    var text = target.nodeValue || target.textContent;
    // Return range if span only has one word.
    if (!text.includes(' ')) {
      range.setEnd(target, text.length);
      return range;
    }
    // Now, let's split its content to words
    var words = text.split(' '),
      textNode, newText;
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
        range.setEnd(textNode, newText.length);
        return range;
      }
      i++;
    }
  };

  const findTitlesAndLanguage = (rendition, location) => {
    let book = rendition.book
    let spineItem = book.spine.get(location);
    let bookTitle = book.package.metadata.title
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

    const [chapterTitle, language] = findTitlesAndLanguage(rendition, location);
    sendMessage({method:"set", key: "chapterTitle", value: chapterTitle});
    sendMessage({method:"set", key: "language", value: language});
    return language;
  }

  function setWordInformation(highlightedContent, context, epubcfi, morphology, fromTop) {
    if (highlightedContent && fromTop) {
      sendMessageWithoutCache({method:"set", key: "highlightedContent", jsonValue: JSON.stringify({text: highlightedContent, fromTop: fromTop})});
    } else {
      sendMessageWithoutCache({method:"set", key: "highlightedContent", value: null});
    }
    if (context) {
      sendMessageWithoutCache({method:"set", key: "context", jsonValue: JSON.stringify(context)});
    }
    sendMessage({method:"set", key: "epubcfi", value: epubcfi});
    sendMessage({method:"set", key: "morphology", jsonValue: JSON.stringify(morphology)});
  }

  function renditionHandler(rendition, location) {
    setTitlesAndLanguage(rendition, location);
  }
  function _ready() {
    var contents;
    var targetOrigin = "*";

    var q = [];
    var _isReady = false;

    var book;
    var rendition;

    var minSpreadWidth = 815;
    var axis = "horizontal";

    var isChrome = /Chrome/.test(navigator.userAgent);
    var isWebkit = !isChrome && /AppleWebKit/.test(navigator.userAgent);

    let pageBegin = null;
    let pageEnd = null;
    let startWord = null;
    let pageJustTurned = false;
    function onMessage(e) {
      var message = e.data;
      handleMessage(message);
    }

    function getSmil(location, resultFn) {
      const idref = book.packaging.spine[location.start.index].idref;
      const overlay = book.packaging.manifest[idref].overlay;
      const smil = book.packaging.manifest[overlay]
      if(smil) {
        book.load(smil.href).then(function(smilFile) {
          let parser = new DOMParser();
          let xmlDoc = parser.parseFromString(smilFile, "text/xml");
          resultFn(xmlDoc, overlay);
        });
      } else {
        resultFn(null);
      }

    }

    function timeStampForRef(xmlDoc, ref) {
      let sequence = sequenceForRef(xmlDoc, ref);
      let clip = sequence.querySelector("audio")
      return clip;
    }

    function timeStampForRefLast(xmlDoc, ref) {
      let sequence = sequenceForRef(xmlDoc, ref);
      let audioClips = sequence.querySelectorAll("audio")
      return audioClips[audioClips.length - 1];
    }

    function sequenceForRef(xmlDoc, ref) {
      let sequences = xmlDoc.children[0].children[0].children[0];
      let documentName = sequences.attributes["epub:textref"].value;
      let sequence = sequences.querySelector(`seq[*|textref=${documentName}\\#${ref}]`)
      return sequence;
    }

    function setCurrentSentence(location, time) {
      let startingParagraphNumber = parseInt(startWord.slice(1, 7));
      let startingSentenceNumber = parseInt(startWord.slice(8, 14));
      let iframeDoc = Array.from(document.getElementsByClassName("epub-container")[0].children);
      let sentences = iframeDoc.find((e) => e.children.length > 0).children[0].contentWindow.document.getElementsByClassName("sentence");
      let sentence = getSmil(location, (xmlDoc, overlay) => {
        return Array.from(sentences).find((s) => {
          let paragraphNumber = parseInt(s.id.slice(1, 7));
          let sentenceNumber = parseInt(s.id.slice(8, 14));
          if (paragraphNumber > startingParagraphNumber || (sentenceNumber >= startingSentenceNumber && paragraphNumber == startingParagraphNumber)) {
            let sentenceEndTime = timeStampForRefLast(xmlDoc, s.id).getAttribute("clipEnd");
            let segments = sentenceEndTime.split(":");
            let hours = parseFloat(segments[0])
            let minutes = parseFloat(segments[1])
            let seconds = parseFloat(segments[2])

            let endSeconds = (hours * 3600.0) + (minutes * 60.0) + seconds
            if(endSeconds > time + 0.5) {
              s.classList.add("active-audio");
              return true;
            } else {
              s.classList.remove("active-audio");
              return false;
            }
          } else {
            s.classList.remove("active-audio");
            return false;
          }
        });
      });
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
        case "currentAudioTime": {
          var args = decoded.args && decoded.args.length && decoded.args[0];
          console.log("BRIDGE TIME", args.audioTime);
          let segments = pageEnd.split(":");
          let hours = parseFloat(segments[0])
          let minutes = parseFloat(segments[1])
          let seconds = parseFloat(segments[2])
          let endTime = (hours * 3600.0) + (minutes * 60.0) + seconds
          setCurrentSentence(rendition.location, args.audioTime);
          if (args.audioTime + 0.2 > endTime && !pageJustTurned) {
            rendition.next();
          } else if(pageJustTurned) {
            pageJustTurned = false;
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
       case "deselect": {
         if (window.contents) {
           window.contents.emit("deselected");
         }
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

      window.book = book = ePub(url);

      window.rendition = rendition = book.renderTo(document.body, settings);
      
      rendition.hooks.content.register(function(contents, rendition) {
        window.contents = contents;
        contents.triggerSelectedEvent = function(cfi, range, context){
          if(cfi) {
            this.previouslySelected = true;
            // cfirange = this.section.cfiFromRange(range);
            this.emit("selected", {cfi, range, context});
            //this.emit("selectedRange", range);
          } else if (this.previouslySelected) {
            this.previouslySelected = false;
            this.emit("deselected");
          }
        }

        contents.on("deselected", function() {
          setWordInformation(null, null, null, null, null);
          let svg = document.getElementById("select-box");
          svg.style.visibility = "hidden";
        });

        var doc = contents.document;
        var startPosition = { x: -1, y: -1 };
        var currentPosition = { x: -1, y: -1 };
        var isLongPress = false;
        var longPressTimer;
        var touchduration = 300;
        var preventTap;
        var $body = doc.getElementsByTagName('body')[0];
        var previous = "";
        var range = document.createRange();

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

                sendMessageWithoutCache({method:"longpress", position: currentPosition, cfi: cfi});
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

            sendMessageWithoutCache({method:"longpress", position: currentPosition, cfi: cfi});
            preventTap = true;
          }, touchduration);
        }

        function touchMoveHandler(e) {
          let svg = document.getElementById("select-box");
          setWordInformation(null, null, null, null, null);
          range = document.createRange();
          svg.style.visibility = "hidden";
          currentPosition.x = e.targetTouches[0].pageX;
          currentPosition.y = e.targetTouches[0].pageY;
          clearTimeout(longPressTimer);
        }

        function touchEndHandler(e) {
          var txt = document.getSelection().toString();
          if (txt || previous && !(txt && previous)) {
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

            range = getWordRange(e);

                        /*
            if (range && !lastRange.collapsed && !multiwordSet) {
              var lastY = lastRange.getBoundingClientRect().y;
              var currentY = range.getBoundingClientRect().y;
              var comparison = range.compareBoundaryPoints(Range.END_TO_START, lastRange);

              if(Math.abs(currentY - lastY) < 50) {
                multiwordSet = true;
                if (comparison == 1) {
                  range.setStart(lastRange.startContainer, 0);
                } else if (comparison == -1) {
                  range.setEndAfter(lastRange.endContainer, 0);
                }
              }
            } else if (multiwordSet) {
              multiwordSet = false;
            }
            */
            if (range) {
              let context = getSentence(range);
              cfi = contents.cfiFromRange(range);
              contents.triggerSelectedEvent(cfi, range, context);
            } else {
              range = document.createRange();
              contents.triggerSelectedEvent(null, null, null);
              cfi = contents.cfiFromNode(target).toString();

              if(isLongPress) {
                sendMessageWithoutCache({method:"longpress", position: currentPosition, cfi: cfi});
                isLongPress = false;
              } else {
                setTimeout(function() {
                  if(preventTap) {
                    preventTap = false;
                    isLongPress = false;
                    return;
                  }
                  console.log("TAP");
                  sendMessageWithoutCache({method:"press", position: currentPosition, cfi: cfi});
                }, 10);
              }
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

            sendMessageWithoutCache({method:"longpress", position: currentPosition, cfi: cfi});
            isLongPress = false;
            preventTap = true;
          }
        }

        doc.addEventListener("touchstart", touchStartHandler, false);

        doc.addEventListener("touchmove", touchMoveHandler, false);

        doc.addEventListener("touchend", touchEndHandler, false);

        doc.addEventListener('touchforcechange', touchForceHandler, false);


      }.bind(this));

      rendition.on("relocated", function(location){
        console.log("RELOCATED", location);
        //renditionHandler(rendition, options.location);
        getSmil(location, (xmlDoc, overlay) => {
          if (xmlDoc) {
            startWord = rendition.getRange(location.start.cfi).commonAncestorContainer.parentNode.id || "p000001s000001w000001";
            let endNode = rendition.getRange(location.end.cfi).commonAncestorContainer;
            let endWord = null;
            if (endNode.parentNode.classList.contains("sentence") || endNode.parentNode.classList.contains("paragraph")) {
              endWord = endNode.previousSibling.id;
            }
            else {
              endWord = endNode.parentNode.id;
            }
            pageBegin = timeStampForRef(xmlDoc, startWord).getAttribute("clipBegin");
            pageEnd = timeStampForRef(xmlDoc, endWord).getAttribute("clipEnd");
            let index = parseInt(overlay.replace("smil-", ""), 10);
            pageJustTurned = true;
            sendMessageWithoutCache({method:"relocated", location: location, pageBegin: pageBegin, pageEnd: pageEnd, smilChapter: index});
          }
          else {
            sendMessageWithoutCache({method:"relocated", location: location, pageBegin: null, pageEnd: null});
          }
        });

        const [chapterTitle, language] = findTitlesAndLanguage(rendition, options.location);
        sendMessage({method:"set", key: "chapterTitle", value: chapterTitle});
      });

      rendition.on("selected", function({cfi, range, context}, contents, t) {
        let span = range.startContainer;
        let text = range.toString();
        let svg = document.getElementById("select-box");
        let rectangle = svg.children[0];
        svg.style.width = range.getBoundingClientRect().width + 3;
        svg.style.height = range.getBoundingClientRect().height + 3;
        svg.style.left = range.getBoundingClientRect().x % rendition._layout.columnWidth;
        svg.style.top = range.getBoundingClientRect().y;
        svg.style.visibility = "visible";
        if (span && span.tagName.toLowerCase() == 'span' ) {
          setWordInformation(text, context, range, span.dataset, range.getBoundingClientRect().top);
        } else {
          setWordInformation(text, context, range, null, range.getBoundingClientRect().top);
        }
      });

      rendition.on("markClicked", function (cfiRange, data) {
        sendMessage({method:"markClicked", cfiRange: cfiRange, data: data});
      });

      rendition.on("rendered", function (section) {
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
