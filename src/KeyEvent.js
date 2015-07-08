/* global exports */

// module KeyEvent


    "use strict";

    exports.onKeyDown = function(handleKeyDown) {
      return function() {
        document.onkeydown = function(event) {handleKeyDown(event)()};
        };}

    exports.onKeyUp = function(handleKeyUp) {
      return function() {
        document.onkeyup = function(event) {handleKeyUp(event)()};
        };}

    exports.eventGetKeyCode  = function(event) {
      return (event.keyCode);
      }
