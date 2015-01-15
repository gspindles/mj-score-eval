// Tile images 

var tileimg = {
  'C1' : '../img/C1.jpg',
  'C2' : '../img/C2.jpg',              
  'C3' : '../img/C3.jpg',
  'C4' : '../img/C4.jpg',
  'C5' : '../img/C5.jpg',
  'C6' : '../img/C6.jpg',
  'C7' : '../img/C7.jpg',
  'C8' : '../img/C8.jpg',
  'C9' : '../img/C9.jpg',

  'B1' : '../img/B1.jpg',
  'B2' : '../img/B2.jpg',              
  'B3' : '../img/B3.jpg',
  'B4' : '../img/B4.jpg',
  'B5' : '../img/B5.jpg',
  'B6' : '../img/B6.jpg',
  'B7' : '../img/B7.jpg',
  'B8' : '../img/B8.jpg',
  'B9' : '../img/B9.jpg',

  'K1' : '../img/K1.jpg',
  'K2' : '../img/K2.jpg',              
  'K3' : '../img/K3.jpg',
  'K4' : '../img/K4.jpg',
  'K5' : '../img/K5.jpg',
  'K6' : '../img/K6.jpg',
  'K7' : '../img/K7.jpg',
  'K8' : '../img/K8.jpg',
  'K9' : '../img/K9.jpg',

  'W1' : '../img/W1.jpg',
  'W2' : '../img/W2.jpg',              
  'W3' : '../img/W3.jpg',
  'W4' : '../img/W4.jpg',
 
  'D1' : '../img/D1.jpg',
  'D2' : '../img/D2.jpg',              
  'D3' : '../img/D3.jpg',
 
  'F1' : '../img/F1.jpg',
  'F2' : '../img/F2.jpg',              
  'F3' : '../img/F3.jpg',
  'F4' : '../img/F4.jpg',
 
  'S1' : '../img/S1.jpg',
  'S2' : '../img/S2.jpg',              
  'S3' : '../img/S3.jpg',
  'S4' : '../img/S4.jpg',

  'Empty' : '../img/Empty.jpg'
};

function getImg(tile) {
  return "<img src=\"" + tileimg[tile] + "\" alt=\"" + tile + "style=\"width : 70; height : 91\">";
}


// Tile unicode text

var tiletxt = {
  'C1' : '\u127001',
  'C2' : '\u127002',
  'C3' : '\u127003',
  'C4' : '\u127004',
  'C5' : '\u127005',
  'C6' : '\u127006',
  'C7' : '\u127007',
  'C8' : '\u127008',
  'C9' : '\u127009',

  'B1' : '\u127992',
  'B2' : '\u127993',
  'B3' : '\u127994',
  'B4' : '\u127995',
  'B5' : '\u127996',
  'B6' : '\u127997',
  'B7' : '\u127998',
  'B8' : '\u127999',
  'B9' : '\u127000',

  'K1' : '\u126983',
  'K2' : '\u126984',
  'K3' : '\u126985',
  'K4' : '\u126986',
  'K5' : '\u126987',
  'K6' : '\u126988',
  'K6' : '\u126989',
  'K8' : '\u126990',
  'K9' : '\u126991',

  'W1' : '\u126976',
  'W2' : '\u126977',
  'W3' : '\u126978',
  'W4' : '\u126979',

  'D1' : '\u126980',
  'D2' : '\u126981',
  'D3' : '\u126982',

  'F1' : '\u127010',
  'F2' : '\u127011',
  'F3' : '\u127012',
  'F4' : '\u127013',

  'S1' : '\u127014',
  'S2' : '\u127015',
  'S3' : '\u127016',
  'S4' : '\u127017',

  'Joker' : '\u127018',
  'Empty' : '\u127019'
}

function getText(tile) {
  return tiletxt[tile];
}


// Tile definations

var coins = ['C1', 'C2', 'C3', 'C4', 'C5', 'C6', 'C7', 'C8', 'C9'];
var bamboos = ['B1', 'B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'B8', 'B9'];
var characters = ['K1', 'K2', 'K3', 'K4', 'K5', 'K6', 'K7', 'K8', 'K9'];
var winds = ['W1', 'W2', 'W3', 'W4'];
var dragons = ['D1', 'D2', 'D3'];
var flowers = ['F1', 'F2', 'F3', 'F4'];
var seasons = ['S1', 'S2', 'S3', 'S4'];
var simples = coins.slice(1, 8) + bamboos.slice(1, 8) + characters.slice(1, 8);
var terminals = ['C1', 'C9', 'B1', 'B9', 'K1', 'K9'];
var honors = winds + dragons;
var edges = terminals + honors;
var greens = ['B2', 'B3', 'B4', 'B6', 'B8', 'D2'];
var reds = ['B1', 'B5', 'B7', 'B9', 'D1'];
var blues = ['C8', 'W1', 'W2', 'W3', 'W4', 'D3'];

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
  if (l[0] == 'C' || l[0] == 'B' || l[0] == 'K') {
    if (parseInt(l[1]) < 8) {
      return [tile, dora(tile), dora(dora(tile))];
    }
    else {
      return [l[0] + 7, l[0] + 8, l[0] + 9];
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
  return tiles[type][Math.floor(Math.random() * tiles[type].length)];
}

function makeMeld(type, meld) {
  if (type == 'C') {
     if (meld == 'c') {
      return makeChow(getTileFrom('C'));
    }
    else if (meld == 'p') {
      return makePung(getTileFrom('C'));
    }
    else if (meld == 'k') {
      return makeKong(getTileFrom('C'));
    }
    else {
      return makeEye(getTileFrom('C'));
    }
  }
  else if (type == 'B') {
    if (meld == 'c') {
      return makeChow(getTileFrom('B'));
    }
    else if (meld == 'p') {
      return makePung(getTileFrom('B'));
    }
    else if (meld == 'k') {
      return makeKong(getTileFrom('B'));
    }
    else {
      return makeEye(getTileFrom('B'));
    }
  }
  else if (type == 'K') {
    if (meld == 'c') {
      return makeChow(getTileFrom('K'));
    }
    else if (meld == 'p') {
      return makePung(getTileFrom('K'));
    }
    else if (meld == 'k') {
      return makeKong(getTileFrom('K'));
    }
    else {
      return makeEye(getTileFrom('K'));
    }
  }
  else if (type == 'W') {
    if (meld == 'p') {
      return makePung(getTileFrom('W'));
    }
    else if (meld == 'k') {
      return makeKong(getTileFrom('W'));
    }
    else {
      return makeEye(getTileFrom('W'));
    }
  }
  else if (type == 'D') {
    if (meld == 'p') {
      return makePung(getTileFrom('D'));
    }
    else if (meld == 'k') {
      return makeKong(getTileFrom('D'));
    }
    else {
      return makeEye(getTileFrom('D'));
    }
  }
  else if (type == 'F') {
    return flowers;
  }
  else if (type == 'S') {
    return seasons;
  }
  else if (type == 's') {
    if (meld == 'c') {
      return makeChow(getTileFrom('s'));
    }
    else if (meld == 'p') {
      return makePung(getTileFrom('s'));
    }
    else if (meld == 'k') {
      return makeKong(getTileFrom('s'));
    }
    else {
      return makeEye(getTileFrom('s'));
    }
  }
  else if (type == 'T') {
    if (meld == 'c') {
      return makeChow(getTileFrom('T'));
    }
    else if (meld == 'p') {
      return makePung(getTileFrom('T'));
    }
    else if (meld == 'k') {
      return makeKong(getTileFrom('T'));
    }
    else {
      return makeEye(getTileFrom('T'));
    }
  }
  else if (type == 'H') {
    if (meld == 'p') {
      return makePung(getTileFrom('H'));
    }
    else if (meld == 'k') {
      return makeKong(getTileFrom('H'));
    }
    else {
      return makeEye(getTileFrom('H'));
    }
  }
  else if (type == 'E') {
    if (meld == 'p') {
      return makePung(getTileFrom('E'));
    }
    else if (meld == 'k') {
      return makeKong(getTileFrom('E'));
    }
    else {
      return makeEye(getTileFrom('E'));
    }
  }
  else if (type == 'r') {
    if (meld == 'p') {
      return makePung(getTileFrom('r'));
    }
    else if (meld == 'k') {
      return makeKong(getTileFrom('r'));
    }
    else {
      return makeEye(getTileFrom('r'));
    }
  }
  else if (type == 'g') {
    if (meld == 'p') {
      return makePung(getTileFrom('g'));
    }
    else if (meld == 'k') {
      return makeKong(getTileFrom('g'));
    }
    else {
      return makeEye(getTileFrom('g'));
    }
  }
  else {
    if (meld == 'p') {
      return makePung(getTileFrom('b'));
    }
    else if (meld == 'k') {
      return makeKong(getTileFrom('b'));
    }
    else {
      return makeEye(getTileFrom('b'));
    }
  }
}

function makeImg(meld) {
  return map(getImg, meld);
}

function makeTxt(meld) {
  return map(getImg, meld);
}


// Generating functions for different hands

function makeHand(melds) {
  hand = [];
  for (var i = 0; i < hand.length; i++) {
    hand.push(makeMeld(melds[i][0], melds[i][1]));
  }
  return hand;
}

function makeChicken() {
  // to make this easier, just have a chow from each suit, a suit pung, and wind eyes
  return makeHand([['C','c'],['B','c'],['K','c'],['S','p'],['W','e']]);
}

function makeAllSimples() {
  return makeHand([['s','c'],['s','c'],['s','p'],['s','p'],['s','e']]);
}

function makeAllTypes() {
}



// utility functions

function repeat(item, n) {
  var l = [];
  for (var i = 0; i < n; i++) {
    l.push(item);
  }
  return l;
}

function map(func, list) {
  l = [];
  for (var i = 0; i < list.length; i++) {
    l.push(func(list[i]));
  }
  return l;
}

function dora(tile) {
  l = tile.split('');
  v = parseInt(l[1]);
  if (l[0] == 'C' || l[0] == 'B' || l[0] == 'K') {
    if (v < 9) {
      return l[0] + (v + 1);
    }
    return l[0] + 1;
  }
  if (l[0] == 'W' || l[0] == 'F' || l[0] == 'S') {
    if (v < 4) {
      return l[0] + (v + 1);
    }
    return l[0] + 1;
  }
  else {
    if (l[1] < 3) {
      return l[0] + (v + 1);
    }
    return l[0] + 1;
  }
}
