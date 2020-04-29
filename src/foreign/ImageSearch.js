var Config = require("react-native-config").default;

/**
 * Retrieves a list of image search results from Google
 * @param (String) searchTerm
 * @param (Number) -optional- start (starting from what result)
 * @param (Number) -optional- num (how many results to return, 1 - 10)
 *
 * @return (Object)
 *
 */

let getImageSearchResults = function(searchTerm, start, num) {
  start = start < 0 || start > 90 || typeof start === "undefined" ? 0 : start;
  num = num < 1 || num > 10 || typeof num === "undefined" ? 10 : num;

  if (!searchTerm) {
    console.error("No search term");
  }

  let parameters = "&q=" + encodeURIComponent(searchTerm);
  parameters += "&searchType=image";
  parameters += start ? "&start=" + start : "";
  parameters += "&num=" + num;
  parameters += "&imgSize=" + "large";
  parameters += "&fields=" + "items";

  let host = "https://www.googleapis.com";
  let path =
    "/customsearch/v1?key=" +
    Config.CSE_API_KEY +
    "&cx=" +
    Config.CSE_ID +
    parameters;

  return fetch(host + path).then(response => response.json()).then(json => {
    let resultsArray = [];
    // check for usage limits (contributed by @ryanmete)
    // This handles the exception thrown when a user's Google CSE quota has been exceeded for the day.
    // Google CSE returns a JSON object with a field called "error" if quota is exceed.
    if (json.error && json.error.errors) {
      resultsArray.push(json.error.errors[0]);

      // returns the JSON formatted error message in the callback
      throw resultsArray;
    } else if (json.items) {
      // search returned results
      json.items.forEach(function(item) {
        resultsArray.push(item);
      });

      return resultsArray;
    } else {
      return [];
    }
  });
}

exports._imageSearch = function(keyword) {
  return function (low) {
    return function (high) {
      return function () {
        return getImageSearchResults(keyword, low, high);
      }
    }
  }
}
