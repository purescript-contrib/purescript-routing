// module Routing

exports.hashChanged = function(handler) {
    return function() {
        var getHash = function() {
            return document.location.href.split('#').splice(1).join('#');
        };
        var oldHash = '';
        handler('')(getHash())();
        window.addEventListener('hashchange', function(ev) {
            var newHash = getHash();
            handler(oldHash)(newHash)();
            oldHash = newHash;
        });
    };
};

exports.decodeURIComponent = function(str) {
    if (typeof window !== "undefined") {
        return window.decodeURIComponent(str);
    } else {
        return global.decodeURIComponent(str);
    }
};
