/* global exports */

// module Control.Monad.Eff.Alert


    "use strict";

    exports.alert = function(msg) {
        return function() {
          window.alert(msg);
          return {};
        };
    };
