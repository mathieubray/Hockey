var webPage = require('webpage');
var page = webPage.create();

var fs = require('fs');
var path = 'C:/Users/braym/Box Sync/Personal/Hockey/NCAA/Data/Scrapers/rpi.html'

page.open('http://www.uscho.com/rankings/rpi/d-i-men/', function (status) {
  var content = page.content;
  fs.write(path,content,'w')
  phantom.exit();
});