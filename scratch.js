console.log(\"***START\")
(function() {
function getSelectionAndSendMessage() {
    console.log(\"***JS EVENT\");
    var txt = document.getSelection().toString() ;
    window.webkit.messageHandlers.%@.postMessage(txt) ;
}
console.log(\"***JS START\");
document.onmouseup = getSelectionAndSendMessage ;
document.onkeyup   = getSelectionAndSendMessage ;
document.oncontextmenu  = getSelectionAndSendMessage ;
document.ontouchstart  = getSelectionAndSendMessage ;
})() 
