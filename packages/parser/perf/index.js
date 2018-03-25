const { readFileSync } = require('fs');
const { join } = require('path');
const Benchmark = require('benchmark');

const padl = (n, s) => {
  let res = s;
  while(res.length < n) {
    res += ' ';
  }

  return res;
};

const padr = (n, s) => {
  let res = s;
  while (res.length < n) {
    res = ' ' + res;
  }

  return res;
};

const sweetsourParser = require('..');
const Stylis = require('stylis');
const makeInsertRulePlugin = require('stylis-rule-sheet');

const normalizeRaw = readFileSync(join(__dirname, '../__tests__/suite/normalize.css')).toString('utf8');
const suite = Benchmark.Suite('css parser');

const stylisSplitter = new Stylis({
  global: false,
  cascade: false,
  keyframe: false,
  prefix: false,
  compress: false,
  semicolon: true,
});

let parsingRules = [];
const returnRulesPlugin = context => {
  if (context === -2) {
    const parsedRules = parsingRules;
    parsingRules = [];
    return parsedRules;
  }
};

const parseRulesPlugin = makeInsertRulePlugin(rule => {
  parsingRules.push(rule);
});

stylisSplitter.use([parseRulesPlugin, returnRulesPlugin]);

const options = {
  onError: e => { e.currentTarget.failure = e.error; }
};

suite
  .add('sweetsour normalize.css', () => {
    sweetsourParser.nodeStreamToOutput(
      sweetsourParser.parseTemplate([normalizeRaw], [])
    );
  }, options)
  .add('stylis normalize.css', () => {
    stylisSplitter('', normalizeRaw);
  }, options)

function logStart () {
  console.log(this.name);
  console.log('-----------------------------------------------');
}

function logResults (e) {
  const t = e.target;

  if (t.failure) {
    console.error(padl(10, t.name) + 'FAILED: ' + e.target.failure);
  } else {
    const result = padl(10, t.name)
      + padr(13, t.hz.toFixed(2) + ' op/s')
      + ' \xb1' + padr(7, t.stats.rme.toFixed(2) + '%')
      + padr(15, ' (' + t.stats.sample.length + ' samples)');

    console.log(result);
  }
}

function logComplete () {
  console.log('-----------------------------------------------');
}

suite
  .on('start', logStart)
  .on('cycle', logResults)
  .on('complete', logComplete)
  .run({ 'async': true });
