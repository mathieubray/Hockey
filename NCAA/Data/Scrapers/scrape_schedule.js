var webPage = require('webpage');
var page = webPage.create();

var fs = require('fs');
var path = 'C:/Users/braym/Box Sync/Personal/Hockey/NCAA/Data/Scrapers/schedule.html'

page.open('http://www.uscho.com/scoreboard/division-i-men/2017-2018/composite-schedule/', function (status) {
  var content = page.content;
  fs.write(path,content,'w')
  phantom.exit();
});