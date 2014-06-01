// link to all the images

var tileimg = {
  'c1' : '../img/c1.jpg',
  'c2' : '../img/c2.jpg',              
  'c3' : '../img/c3.jpg',
  'c4' : '../img/c4.jpg',
  'c5' : '../img/c5.jpg',
  'c6' : '../img/c6.jpg',
  'c7' : '../img/c7.jpg',
  'c8' : '../img/c8.jpg',
  'c9' : '../img/c9.jpg',

  'b1' : '../img/b1.jpg',
  'b2' : '../img/b2.jpg',              
  'b3' : '../img/b3.jpg',
  'b4' : '../img/b4.jpg',
  'b5' : '../img/b5.jpg',
  'b6' : '../img/b6.jpg',
  'b7' : '../img/b7.jpg',
  'b8' : '../img/b8.jpg',
  'b9' : '../img/b9.jpg',

  'k1' : '../img/k1.jpg',
  'k2' : '../img/k2.jpg',              
  'k3' : '../img/k3.jpg',
  'k4' : '../img/k4.jpg',
  'k5' : '../img/k5.jpg',
  'k6' : '../img/k6.jpg',
  'k7' : '../img/k7.jpg',
  'k8' : '../img/k8.jpg',
  'k9' : '../img/k9.jpg',

  'w1' : '../img/w1.jpg',
  'w2' : '../img/w2.jpg',              
  'w3' : '../img/w3.jpg',
  'w4' : '../img/w4.jpg',
 
  'd1' : '../img/d1.jpg',
  'd2' : '../img/d2.jpg',              
  'd3' : '../img/d3.jpg',
 
  'f1' : '../img/f1.jpg',
  'f2' : '../img/f2.jpg',              
  'f3' : '../img/f3.jpg',
  'f4' : '../img/f4.jpg',
 
  's1' : '../img/s1.jpg',
  's2' : '../img/s2.jpg',              
  's3' : '../img/s3.jpg',
  's4' : '../img/s4.jpg'
};


// All the chows

var c1c2c3 = ['c1', 'c2', 'c3'];
var c2c3c4 = ['c2', 'c3', 'c4'];
var c3c4c5 = ['c3', 'c4', 'c5'];
var c4c5c6 = ['c4', 'c5', 'c6'];
var c5c6c7 = ['c5', 'c6', 'c7'];
var c6c7c8 = ['c6', 'c7', 'c8'];
var c7c8c9 = ['c7', 'c8', 'c9'];

var coinSimpleChows = [c2c3c4, c3c4c5, c4c5c6, c5c6c7, c6c7c8];
var coinTerminalChows = [c1c2c3, c7c8c9];
var coinChows = coinSimpleChows + coinTerminalChows;

var b1b2b3 = ['b1', 'b2', 'b3'];
var b2b3b4 = ['b2', 'b3', 'b4'];
var b3b4b5 = ['b3', 'b4', 'b5'];
var b4b5b6 = ['b4', 'b5', 'b6'];
var b5b6b7 = ['b5', 'b6', 'b7'];
var b6b7b8 = ['b6', 'b7', 'b8'];
var b7b8b9 = ['b7', 'b8', 'b9'];

var bambooSimpleChows = [b2b3b4, b3b4b5, b4b5b6, b5b6b7, b6b7b8];
var bambooTerminalChows = [b1b2b3, b7b8b9];
var bambooChows = bambooSimpleChows + bambooTerminalChows;

var k1k2k3 = ['k1', 'k2', 'k3'];
var k2k3k4 = ['k2', 'k3', 'k4'];
var k3k4k5 = ['k3', 'k4', 'k5'];
var k4k5k6 = ['k4', 'k5', 'k6'];
var k5k6k7 = ['k5', 'k6', 'k7'];
var k6k7k8 = ['k6', 'k7', 'k8'];
var k7k8k9 = ['k7', 'k8', 'k9'];

var characterSimpleChows = [k2k3k4, k3k4k5, k4k5k6, k5k6k7, k6k7k8];
var characterTerminalChows = [k1k2k3, k7k8k9];
var characterChows = characterSimpleChows + characterTerminalChows;

var simpleChows = coinSimpleChows + bambooSimpleChows + characterSimpleChows;
var terminalChows = coinTerminalChows + bambooTerminalChows + characterTerminalChows;
var allChows = allSimpleChows + allTerminalChows;


// All the pungs

var c1c1c1 = ['c1', 'c1', 'c1'];
var c2c2c2 = ['c2', 'c2', 'c2'];
var c3c3c3 = ['c3', 'c3', 'c3'];
var c4c4c4 = ['c4', 'c4', 'c4'];
var c5c5c5 = ['c5', 'c5', 'c5'];
var c6c6c6 = ['c6', 'c6', 'c6'];
var c7c7c7 = ['c7', 'c7', 'c7'];
var c8c8c8 = ['c8', 'c8', 'c8'];
var c9c9c9 = ['c9', 'c9', 'c9'];

var coinSimplePungs = [c2c2c2, c3c3c3, c4c4c4, c5c5c5, c6c6c6, c7c7c7, c8c8c8];
var coinTerminalPungs = [c1c1c1, c9c9c9];
var coinPungs = coinSimplePungs + coinTerminalPungs;

var b1b1b1 = ['b1', 'b1', 'b1'];
var b2b2b2 = ['b2', 'b2', 'b2'];
var b3b3b3 = ['b3', 'b3', 'b3'];
var b4b4b4 = ['b4', 'b4', 'b4'];
var b5b5b5 = ['b5', 'b5', 'b5'];
var b6b6b6 = ['b6', 'b6', 'b6'];
var b7b7b7 = ['b7', 'b7', 'b7'];
var b8b8b8 = ['b8', 'b8', 'b8'];
var b9b9b9 = ['b9', 'b9', 'b9'];

var bambooSimplePungs = [b2b2b2, b3b3b3, b4b4b4, b5b5b5, b6b6b6, b7b7b7, b8b8b8];
var bambooTerminalPungs = [b1b1b1, b9b9b9];
var bambooPungs = bambooSimplePungs + bambooTerminalPungs;

var k1k1k1 = ['k1', 'k1', 'k1'];
var k2k2k2 = ['k2', 'k2', 'k2'];
var k3k3k3 = ['k3', 'k3', 'k3'];
var k4k4k4 = ['k4', 'k4', 'k4'];
var k5k5k5 = ['k5', 'k5', 'k5'];
var k6k6k6 = ['k6', 'k6', 'k6'];
var k7k7k7 = ['k7', 'k7', 'k7'];
var k8k8k8 = ['k8', 'k8', 'k8'];
var k9k9k9 = ['k9', 'k9', 'k9'];

var characterSimplePungs = [k2k2k2, k3k3k3, k4k4k4, k5k5k5, k6k6k6, k7k7k7, k8k8k8];
var characterTerminalPungs = [k1k1k1, k9k9k9];
var characterPungs = characterSimplePungs + characterTerminalPungs;

var w1w1w1 = ['w1', 'w1', 'w1'];
var w2w2w2 = ['w2', 'w2', 'w2'];
var w3w3w3 = ['w3', 'w3', 'w3'];
var w4w4w4 = ['w4', 'w4', 'w4'];

var windPungs = [w1w1w1, w2w2w2, w3w3w3, w4w4w4];

var d1d1d1 = ['d1', 'd1', 'd1'];
var d2d2d2 = ['d2', 'd2', 'd2'];
var d3d3d3 = ['d3', 'd3', 'd3'];

var dragonPungs = [d1d1d1, d2d2d2, d3d3d3];

var simplePungs = coinSimplePungs + bambooSimplePungs + characterSimplePungs;
var terminalPungs = coinTerminalPungs + bambooTerminalPungs + characterTerminalPungs;
var suitPungs = coinPungs + bambooPungs + dragonPungs;
var honorPungs = dragonPungs + windPungs;
var allPungs = suitPungs + honorPungs; 


// All the kongs

var c1c1c1c1 = ['c1', 'c1', 'c1', 'c1'];
var c2c2c2c2 = ['c2', 'c2', 'c2', 'c2'];
var c3c3c3c3 = ['c3', 'c3', 'c3', 'c3'];
var c4c4c4c4 = ['c4', 'c4', 'c4', 'c4'];
var c5c5c5c5 = ['c5', 'c5', 'c5', 'c5'];
var c6c6c6c6 = ['c6', 'c6', 'c6', 'c6'];
var c7c7c7c7 = ['c7', 'c7', 'c7', 'c7'];
var c8c8c8c8 = ['c8', 'c8', 'c8', 'c8'];
var c9c9c9c9 = ['c9', 'c9', 'c9', 'c9'];

var coinSimpleKongs = [c2c2c2c2, c3c3c3c3, c4c4c4c4, c5c5c5c5, c6c6c6c6, c7c7c7c7, c8c8c8c8];
var coinTerminalKongs = [c1c1c1c1, c9c9c9c9];
var coinKongs = coinSimpleKongs + coinTerminalKongs;

var b1b1b1b1 = ['b1', 'b1', 'b1', 'b1'];
var b2b2b2b2 = ['b2', 'b2', 'b2', 'b2'];
var b3b3b3b3 = ['b3', 'b3', 'b3', 'b3'];
var b4b4b4b4 = ['b4', 'b4', 'b4', 'b4'];
var b5b5b5b5 = ['b5', 'b5', 'b5', 'b5'];
var b6b6b6b6 = ['b6', 'b6', 'b6', 'b6'];
var b7b7b7b7 = ['b7', 'b7', 'b7', 'b7'];
var b8b8b8b8 = ['b8', 'b8', 'b8', 'b8'];
var b9b9b9b9 = ['b9', 'b9', 'b9', 'b9'];

var bambooSimpleKongs = [b2b2b2b2, b3b3b3b3, b4b4b4b4, b5b5b5b5, b6b6b6b6, b7b7b7b6, b8b8b8b8];
var bambooTerminalKongs = [b1b1b1b1, b9b9b9b9];
var bambooKongs = bambooSimpleKongs + bambooTerminalKongs;

var k1k1k1k1 = ['k1', 'k1', 'k1', 'k1'];
var k2k2k2k2 = ['k2', 'k2', 'k2', 'k2'];
var k3k3k3k3 = ['k3', 'k3', 'k3', 'k3'];
var k4k4k4k4 = ['k4', 'k4', 'k4', 'k4'];
var k5k5k5k5 = ['k5', 'k5', 'k5', 'k5'];
var k6k6k6k6 = ['k6', 'k6', 'k6', 'k6'];
var k7k7k7k7 = ['k7', 'k7', 'k7', 'k7'];
var k8k8k8k8 = ['k8', 'k8', 'k8', 'k8'];
var k9k9k9k9 = ['k9', 'k9', 'k9', 'k9'];

var characterSimpleKongs = [k2k2k2k2, k3k3k3k3, k4k4k4k4, k5k5k5k5, k6k6k6k6, k7k7k7k7, k8k8k8k8];
var characterTerminalKongs = [k2k1k1k1, k9k9k9k9];
var characterKongs = characterSimpleKongs + characterTerminalKongs;

var w1w1w1w1 = ['w1', 'w1', 'w1', 'w1'];
var w2w2w2w2 = ['w2', 'w2', 'w2', 'w2'];
var w3w3w3w3 = ['w3', 'w3', 'w3', 'w3'];
var w4w4w4w4 = ['w4', 'w4', 'w4', 'w4'];

var windKongs = [w1w1w1w1, w2w2w2w2, w3w3w3w3, w4w4w4w4];

var d1d1d1d1 = ['d1', 'd1', 'd1', 'd1'];
var d2d2d2d2 = ['d2', 'd2', 'd2', 'd2'];
var d3d3d3d3 = ['d3', 'd3', 'd3', 'd3'];

var dragonKongs = [d1d1d1d1, d2d2d2d2, d3d3d3d3];

var simpleKongs = coinSimpleKongs + bambooSimpleKongs + characterSimpleKongs;
var terminalKongs = coinTerminalKongs + bambooTerminalKongs + characterTerminalKongs;
var suitKongs = coinKongs + bambooKongs + characterKongs;
var honorKongs = dragonKongs + windKongs;
var allKongs = suitKongs + honorKongs; 


// All the eyes

var c1c1 = ['c1', 'c1'];
var c2c2 = ['c2', 'c2'];
var c3c3 = ['c3', 'c3'];
var c4c4 = ['c4', 'c4'];
var c5c5 = ['c5', 'c5'];
var c6c6 = ['c6', 'c6'];
var c7c7 = ['c7', 'c7'];
var c8c8 = ['c8', 'c8'];
var c9c9 = ['c9', 'c9'];

var coinSimpleEyes = [c2c2, c3c3, c4c4, c5c5, c6c6, c7c7, c8c8];
var coinTerminalEyes = [c1c1, c9c9];
var coinEyes = coinSimpleEyes + coinTerminalEyes;

var b1b1 = ['b1', 'b1'];
var b2b2 = ['b2', 'b2'];
var b3b3 = ['b3', 'b3'];
var b4b4 = ['b4', 'b4'];
var b5b5 = ['b5', 'b5'];
var b6b6 = ['b6', 'b6'];
var b7b7 = ['b7', 'b7'];
var b8b8 = ['b8', 'b8'];
var b9b9 = ['b9', 'b9'];

var bambooSimpleEyes = [b2b2, b3b3, b4b4, b5b5, b6b6, b7b6, b8b8];
var bambooTerminalEyes = [b1b1, b9b9];
var bambooEyes = bambooSimpleEyes + bambooTerminalEyes;

var k1k1 = ['k1', 'k1'];
var k2k2 = ['k2', 'k2'];
var k3k3 = ['k3', 'k3'];
var k4k4 = ['k4', 'k4'];
var k5k5 = ['k5', 'k5'];
var k6k6 = ['k6', 'k6'];
var k7k7 = ['k7', 'k7'];
var k8k8 = ['k8', 'k8'];
var k9k9 = ['k9', 'k9'];

var characterSimpleEyes = [k2k2, k3k3, k4k4, k5k5, k6k6, k7k7, k8k8];
var characterTerminalEyes = [k1k1, k9k9];
var characterEyes = characterSimpleEyes = characterTerminalEyes;

var w1w1 = ['w1', 'w1'];
var w2w2 = ['w2', 'w2'];
var w3w3 = ['w3', 'w3'];
var w4w4 = ['w4', 'w4'];

var windEyes = [w1w1, w2w2, w3w3, w4w4];

var d1d1 = ['d1', 'd1'];
var d2d2 = ['d2', 'd2'];
var d3d3 = ['d3', 'd3'];

var dragonEyes = [d1d1, d2d2, d3d3];

var simpleEyes = coinSimpleEyes + bambooSimpleEyes + characterSimpleEyes;
var terminalEyes = coinTerminalEyes + bambooTerminalEyes + characterTerminalEyes;
var suitEyes = coinEyes + bambooEyes + characterEyes;
var honorEyes = dragonEyes + windEyes;
var allEyes = coinEyes + bambooEyes + characterEyes + windEyes + dragonEyes;


// Generating functions for different hands

function makeChicken() {
  // to make this easier, just have a chow from each suit, a suit pung, and wind eyes

}

function makeAllSimples() {

}

function makeAllTypes() {
}



// utility functions
