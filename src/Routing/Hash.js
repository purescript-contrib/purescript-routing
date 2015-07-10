// module Routing.Hash

exports.setHash = function(hash) {
    return function() {
        var uri = document.location.href.split('#')[0];
        document.location.href = uri + '#' + hash;
    };
};

exports.getHash = function() {
    return document.location.href.split('#').splice(1).join('#');
};
