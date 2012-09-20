fs=require('fs');

function printAll(ar) {
  ar.forEach(function(item) {
    console.log(item);
  });
}


var input = fs.readFileSync('/dev/stdin').toString();


printAll(JSON.parse(JSON.parse(input).results));

