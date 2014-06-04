// Tile images 

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
  's4' : '../img/s4.jpg',

  'empty' : '../img/empty.jpg'
};

function getIng(tile) {
  return "<img src=\"" + tileimg(tile) + "\"alt=\"" + tile + ">";
}


// Tile unicode text

var tiletxt = {
  'c1' : '\u127001',
  'c2' : '\u127002',
  'c3' : '\u127003',
  'c4' : '\u127004',
  'c5' : '\u127005',
  'c6' : '\u127006',
  'c7' : '\u127007',
  'c8' : '\u127008',
  'c9' : '\u127009',

  'b1' : '\u127992',
  'b2' : '\u127993',
  'b3' : '\u127994',
  'b4' : '\u127995',
  'b5' : '\u127996',
  'b6' : '\u127997',
  'b7' : '\u127998',
  'b8' : '\u127999',
  'b9' : '\u127000',

  'k1' : '\u126983',
  'k2' : '\u126984',
  'k3' : '\u126985',
  'k4' : '\u126986',
  'k5' : '\u126987',
  'k6' : '\u126988',
  'k6' : '\u126989',
  'k8' : '\u126990',
  'k9' : '\u126991'
  '  '  
  'w1' : '\u126976',
  'w2' : '\u126977',
  'w3' : '\u126978',
  'w4' : '\u126979',

  'd1' : '\u126980',
  'd2' : '\u126981',
  'd3' : '\u126982',

  'f1' : '\u127010',
  'f2' : '\u127011',
  'f3' : '\u127012',
  'f4' : '\u127013',

  's1' : '\u127014',
  's2' : '\u127015',
  's3' : '\u127016',
  's4' : '\u127017',

  'joker' : '\u127018',
  'empty' : '\u127019'
}

function getText(tile) {
  return tiletxt(tile);
}


// Tile definations

var coins = ['c1', 'c2', 'c3', 'c4', 'c5', 'c6', 'c7', 'c8', 'c9'];
var bamboos = ['b1', 'b2', 'b3', 'b4', 'b5', 'b6', 'b7', 'b8', 'b9'];
var characters = ['k1', 'k2', 'k3', 'k4', 'k5', 'k6', 'k7', 'k8', 'k9'];
var winds = ['w1', 'w2', 'w3', 'w4'];
var dragons = ['d1', 'd2', 'd3'];
var flowers = ['f1', 'f2', 'f3', 'f4'];
var seasons = ['s1', 's2', 's3', 's4'];
var simples = coins.slice(1, 8) + bamboos.slice(1, 8) + characters.slice(1, 8);
var terminals = ['c1', 'c9', 'b1', 'b9', 'k1', 'k9'];
var honors = winds + dragons;
var edges = terminals + honors;
var greens = ['b2', 'b3', 'b4', 'b6', 'b8', 'd2'];
var reds = ['b1', 'b5', 'b7', 'b9', 'd1'];
var blues = ['c8', 'w1', 'w2', 'w3', 'w4', 'd3'];

var tiles = {
  'C' : coins,
  'B' : bamboos,
  'K' : characters,
  'W' : winds,
  'D' : dragons,
  'F' : flowers,
  'S' : seasons,
  's' : simples,
  'T' : terminals,
  'H' : honors,
  'E' : edges,
  'r' : reds,
  'g' : greens,
  'b' : blues
}


// Make melds

function makeChow(tile) {
  l = tile.split('');
  if (l[0] == 'c' || l[0] == 'b' || l[0] == 'k') {
    if (l[1] < 8) {
      return [tile, dora(tile), dora(dora(tile))];
    }
    else {
      suit = tile.split('')[0];
      return [suit + 7, suit + 8, suit + 9];
    }
  }
  return undefined;
}

function makePung(tile) {
  return repeat(tile, 3);
}

function makeKong(tile) {
  return repeat(tile, 4);
}

function makeEye(tile) {
  return repeat(tile, 2);
}


/* more specific make functions
 * type char determines the type of tile
 * 'C' coin
 * 'B' bamboo
 * 'K' character
 * 'W' wind
 * 'D' dragon
 * 'F' flower (unused unless for all flowers)
 * 'S' season (unused unless for all seasons)
 * 's' simple
 * 'T' terminal
 * 'H' honor
 * 'E' edge
 * 'r' red
 * 'g' green
 * 'b' blue
 *
 * meld char determines the type of meld
 * 'c' chow
 * 'p' pung
 * 'k' kong
 * 'e' eye
 */



function getTileFrom(type) {
  return tiles['type'][Math.floor(Math.random() * tiles['type'].length)];
}

function makeCoin(meld) {
}

function makeBamboo(meld) {
}

function makeCharacter(meld) {
}

function makeWind(meld) {
}

function makeDragon(meld) {
}

function makeSimple(meld) {
  if (meld == 'c') {
    return makeChow(getTileFrom('s'));
  }
  else if (meld == 'p') {
    return makePung(getTileFrom('s'));
  }
  else if (meld == 'k') {
    return makeKong(getTileFrom('s'));
  }
  else (meld == 'e') {
    return makeEye(getTileFrom('s'));
  }
}



// Generating functions for different hands

function makeChicken() {
  // to make this easier, just have a chow from each suit, a suit pung, and wind eyes

}

function makeAllSimples() {

}

function makeAllTypes() {
}



// utility functions
function repeat(item, n) {
  var l = [];
  for (var i = 0; i < n; i++) {
    l.append(item);
  }
  return l;
}

function dora(tile) {
  l = tile.split('');
  if (l[0] == 'c' || l[0] == 'b' || l[0] == 'k') {
    if (l[0] < 9) {
      return l[0] + (parseInt(l[1]) + 1);
    }
    return l[0] + 1;
  }
  if (l[0] == 'w' || l[0] == 'f' || l[0] == 's') {
    if (l[0] < 4) {
      return l[0] + (parseInt(l[1]) + 1);
    }
    return l[0] + 1;
  }
  else {
    if (l[0] < 3) {
      return l[0] + (parseInt(l[1]) + 1);
    }
    return l[0] + 1;
  }
}
