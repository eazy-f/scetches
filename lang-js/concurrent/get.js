var http = require('request');

var get = function(url) {
    http.get(url, function(error, response, body) {
	if (error) throw new Error(error.message);
	console.log("got response " + response.statusCode + " from " + url);
    });
};

get("http://bbc.com");
get("http://google.com");
