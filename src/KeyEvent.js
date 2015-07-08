/* global exports */

// module KeyEvent


    "use strict";

    exports.onKeyDown = function(handleKeyDown) {
      return function() {
        document.onkeydown = function(event) {handleKeyDown(event)()};
        };
    };

    exports.onKeyUp = function(handleKeyUp) {
      return function() {
        document.onkeyup = function(event) {handleKeyUp(event)()};
        };
    };

    exports.eventGetKeyCode  = function(event) {
      return (event.keyCode);
  };

      exports.getElementByIdFloat = function(targ_id) {
          return function () {
            return parseFloat(document.getElementById(targ_id).value);
          };
      };

        exports.getElementByIdBool = function(targ_id) {
          return function () {
            return document.getElementById(targ_id).checked;
          };
      };
