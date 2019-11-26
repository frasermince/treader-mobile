import { EpubCFI } from "epubjs/lib/index";
const isFirefox = typeof InstallTrigger !== 'undefined';

// TODO Change storage
const commit = (renditionData, snippet, language) => {
  renditionData.mutationFn(
    {
      variables: {
        input: {snippet, language}
      }
    }
  ).then(function(response){
    renditionData.setTranslation((h) => response.data.translate.translation);
  })
}

const toggleVerb = (rendition, renditionData) => {
    renditionData.setHighlightedVerbs(highlightVerbs => ({highlightVerbs: !highlightVerbs})
    storage && storage.setItem("highlight-verbs", renditionData.highlightVerbs);
    rendition.themes.register("new", setTheme(renditionData));
    rendition.themes.select("new");
  }

const toggleNoun = (rendition, renditionData) => {
  renditionData.setHighlightedNouns(highlightNouns => ({highlightNouns: !highlightNouns}),
  this.setState(state => ({highlightNouns: !renditionData.highlightNouns})
  storage && storage.setItem("highlight-nouns", renditionData.highlightNouns);
  rendition.themes.register("new", setTheme(renditionData));
  rendition.themes.select("new");
}

const toggleAdjective = (rendition, renditionData) => {
  renditionData.setHighlightedAdjectives(highlightAdjectives => ({highlightAdjectives: !highlightAdjectives}),
  storage && storage.setItem("highlight-adjectives", renditionData.highlightAdjectives);
  rendition.themes.register("new", setTheme(renditionData));
  rendition.themes.select("new");
}

const setTheme = (renditionData) => {
  let themes = {};

  if (renditionData.highlightVerbs) {
    themes['[data-pos="VERB"]'] = {
      'color': colors["green"]
    }
    themes['[data-pos="AUX"]'] = {
      'color': colors["green"]
    }
  } else {
    themes['[data-pos="VERB"]'] = {
      'color': "black"
    }
    themes['[data-pos="AUX"]'] = {
      'color': "black"
    }
  }
  if (renditionData.highlightNouns) {
    themes['[data-pos="NOUN"]'] = {
      'color': colors["orange"]
    }
    themes['[data-pos="PROPN"]'] = {
      'color': colors["orange"]
    }
  } else {
    themes['[data-pos="NOUN"]'] = {
      'color': "black"
    }
    themes['[data-pos="PROPN"]'] = {
      'color': "black"
    }
  }
  if (renditionData.highlightAdjectives) {
    themes['[data-pos="ADJ"]'] = {
      'color': colors["red"]
    }
  } else {
    themes['[data-pos="ADJ"]'] = {
      'color': "black"
    }
  }
  return themes;
}

const setTitles = (rendition, location, setChapterTitle, setLanguage) => {
  let book = rendition.book
  let locationCfi = location
  let spineItem = book.spine.get(locationCfi);
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
  setChapterTitle((c) => chapterTitle);
  setLanguage((l) => language);
}

function setWordInformation(snippet, epubcfi, wordData) {
  renditionData.setHighlightedContent((s) => snippet);
  renditionData.setEpubcfi((e) => epubcfi);
  renditionData.setMorphology((w) => wordData);
}

export function renditionHandler(rendition, renditionData) {
  rendition.themes.override("line-height", "1.5");
  rendition.hooks.content.register(function(contents, view) {
    contents.triggerSelectedEvent = function(selection){
      var range, cfirange;

      if (selection && selection.rangeCount > 0) {
        range = selection.getRangeAt(0);
        if(!range.collapsed) {
          this.previouslySelected = true;
          // cfirange = this.section.cfiFromRange(range);
          cfirange = new EpubCFI(range, this.cfiBase).toString();
          this.emit("selected", cfirange);
          this.emit("selectedRange", range);
        } else if (this.previouslySelected) {
          this.previouslySelected = false;
          this.emit("deselected");
        }
      }
    }

    contents.on("deselected", function() {
      renditionData.setTranslation((h) => null);
      setWordInformation(null, null, null);
    });
    setTitles(rendition, renditionData.location, renditionData.setChapterTitle, renditionData.setLanguage);
  });

  rendition.themes.default(setTheme(renditionData));

  rendition.on("selected", function(cfiRange) {
    this.book.getRange(cfiRange).then((range) => {
      let span = range.commonAncestorContainer.parentElement; //TODO
      let text = range.toString();
      if (span && span.tagName.toLowerCase() == 'span' ) {
        setWordInformation(text, cfiRange, span.dataset);
        commit(renditionData, text, renditionData.language)
      } else {
        setWordInformation(text, cfiRange, null);
        commit(renditionData, text, renditionData.language)
      }
    });
  });
}
