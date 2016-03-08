// CodeMirror, copyright (c) by Marijn Haverbeke and others
// Distributed under an MIT license: http://codemirror.net/LICENSE

// Coq mode created by Benoît Pin, Valentin Robert and others

(function(mod) {
  if (typeof exports == "object" && typeof module == "object") // CommonJS
    mod(require("../../lib/codemirror"));
  else if (typeof define == "function" && define.amd) // AMD
    define(["../../lib/codemirror"], mod);
  else // Plain browser env
    mod(CodeMirror);
})(function(CodeMirror) {
  "use strict";

  CodeMirror.defineMode('coq', function(_config, _parserConfig) {

    var vernacular = [
      'Abort', 'About', 'Add', 'All', 'Arguments', 'Asymmetric', 'Axiom',
      'Bind',
      'Canonical', 'Check', 'Class', 'Close', 'Coercion', 'CoFixpoint', 'Comments',
      'CoInductive', 'Context', 'Constructors', 'Contextual', 'Corollary',
      'Defined', 'Definition', 'Delimit',
      'Fail',
      'Eval',
      'End', 'Example', 'Export',
      'Fact', 'Fixpoint', 'From',
      'Global', 'Goal', 'Graph',
      'Hint', 'Hypotheses', 'Hypothesis',
      'Implicit', 'Implicits', 'Import', 'Inductive', 'Infix', 'Instance',
      'Lemma', 'Let', 'Local', 'Ltac',
      'Module', 'Morphism',
      'Next', 'Notation',
      'Obligation', 'Open',
      'Parameter', 'Parameters', 'Prenex', 'Print', 'Printing', 'Program',
      'Patterns', 'Projections', 'Proof',
      'Proposition',
      'Qed',
      'Record', 'Relation', 'Remark', 'Require', 'Reserved', 'Resolve', 'Rewrite',
      'Save', 'Scope', 'Search', 'SearchAbout', 'Section', 'Set', 'Show', 'Strict', 'Structure',
      'Tactic', 'Time', 'Theorem', 'Types',
      'Unset',
      'Variable', 'Variables', 'View'
    ];

    var gallina = [
      'as',
      'at',
      'cofix', 'crush',
      'else', 'end',
      'False', 'fix', 'for', 'forall', 'fun',
      'if', 'in', 'is',
      'let',
      'match',
      'of',
      'Prop',
      'return',
      'struct',
      'then', 'True', 'Type',
      'when', 'with'
    ];

    var tactics = [
      'after', 'apply', 'assert', 'auto', 'autorewrite',
      'case', 'change', 'clear', 'compute', 'congruence', 'constructor',
      'congr', 'cut', 'cutrewrite',
      'dependent', 'destruct',
      'eapply', 'eassumption', 'eauto', 'econstructor', 'elim', 'exists',
      'field', 'firstorder', 'fold', 'fourier',
      'generalize',
      'have', 'hnf',
      'induction', 'injection', 'instantiate', 'intro', 'intros', 'inversion',
      'left',
      'move',
      'pattern', 'pose',
      'refine', 'remember', 'rename', 'replace', 'revert', 'rewrite',
      'right', 'ring',
      'set', 'simpl', 'specialize', 'split', 'subst', 'suff', 'symmetry',
      'transitivity', 'trivial',
      'unfold', 'unlock', 'using',
      'vm_compute',
      'where', 'wlog'
    ];

    var terminators = [
      'assumption',
      'by',
      'contradiction',
      'discriminate',
      'exact',
      'now',
      'omega',
      'reflexivity',
      'tauto'
    ];

    var admitters = [
      'admit',
      'Admitted'
    ];

    var words = {};
    vernacular.map(function(word){words[word] = 'builtin';});
    gallina.map(function(word){words[word] = 'keyword';});
    tactics.map(function(word){words[word] = 'tactic';});
    terminators.map(function(word){words[word] = 'terminator';});
    admitters.map(function(word){words[word] = 'keyword';});

//        'let': 'keyword',
//        'print_endline': 'builtin',
//        'true': 'atom',

    function tokenBase(stream, state) {
      if(stream.sol()) {
        state.logicalsol = true; // logicalsol: only \s caracters seen from sol
      }
      if(stream.eol())
        state.logicalsol = false;

      if(stream.eatSpace())
        return null;

      var ch = stream.next();

      if(state.logicalsol) {
        if(/[\*\-\+]/.test(ch)) {
          state.logicalsol = false;
          return 'bullet';
        }
        if(/[\{\}]/.test(ch)) {
          state.logicalsol = false;
          return 'brace';
        }
        if(!(/\s/.test(ch)))
          state.logicalsol = false;
      }

      if(ch === '.') {
        state.tokenize = tokenStatementEnd;
        return state.tokenize(stream, state);
      }

      if (ch === '"') {
        state.tokenize = tokenString;
        return state.tokenize(stream, state);
      }

      if (ch === '(') {
        if (stream.peek() === '*') {
          stream.next();
          state.commentLevel++;
          state.tokenize = tokenComment;
          return state.tokenize(stream, state);
        }
        return 'parenthesis';
      }

      if(ch === ')')
        return 'parenthesis';

      if (ch === '~') {
        stream.eatWhile(/\w/);
        return 'variable-2';
      }

      if (ch === '`') {
        stream.eatWhile(/\w/);
        return 'quote';
      }

      if (/\d/.test(ch)) {
        stream.eatWhile(/[\d]/);
        /*
        if (stream.eat('.')) {
          stream.eatWhile(/[\d]/);
        }
        */
        return 'number';
      }

      if ( /[+\-*&%=<>!?|]/.test(ch)) {
        return 'operator';
      }

      if(/[\[\]]/.test(ch)) {
        return 'bracket';
      }

      stream.eatWhile(/\w/);
      var cur = stream.current();
      return words.hasOwnProperty(cur) ? words[cur] : 'variable';
    }

    function tokenString(stream, state) {
      var next, end = false, escaped = false;
      while ((next = stream.next()) != null) {
        if (next === '"' && !escaped) {
          end = true;
          break;
        }
        escaped = !escaped && next === '\\';
      }
      if (end && !escaped) {
        state.tokenize = tokenBase;
      }
      return 'string';
    }

    function tokenComment(stream, state) {
      var ch;
      while(state.commentLevel && (ch = stream.next())) {
        if(ch === '(' && stream.peek() === '*') {
          stream.next();
          state.commentLevel++;
        }

        if(ch === '*' && stream.peek() === ')') {
          stream.next();
          state.commentLevel--;
        }
      }

      if(!state.commentLevel)
        state.tokenize = tokenBase;

      return 'comment';
    }

    function tokenStatementEnd(stream, state) {
      state.tokenize = tokenBase;
      if(stream.eol() || stream.match(/\s/, false))
        return 'statementend';
    }

    return {
      startState: function() {
        return {tokenize: tokenBase, commentLevel: 0};
      },
      token: function(stream, state) {
        return state.tokenize(stream, state);
      },
      blockCommentStart: "(*",
      blockCommentEnd: "*)",
      lineComment: null
    };
  });

  CodeMirror.defineMIME('text/x-coq', {
    name: 'coq'
  });

});

// Local Variables:
// js-indent-level: 2
// End:
