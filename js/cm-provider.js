var CmCoqProvider;

(function(){
    "use strict";

    var CmSentence = function(start, end, text, is_comment) {
        // start, end: {line: l, ch: c}
        this.start = start;
        this.end   = end;
        this.text  = text;
        this.mark  = undefined;
        this.is_comment = is_comment;
    };

    // A CodeMirror-based Provider of coq statements.
    CmCoqProvider = function(element) {
        this.editor = CodeMirror.fromTextArea(element,
            {mode : {name : "coq",
                     version: 4,
                     singleLineStringErrors : false
                   },
             lineNumbers   : true,
             indentUnit    : 4,
             matchBrackets : true,
             // theme         : 'blackboard',
             keyMap        : "emacs"
            }
        );

        this.editor.on('change', evt => this.onCMChange(evt));
    };

    CmCoqProvider.prototype.focus = function() {
        this.editor.focus();
    };

    // If prev == null then get the first.
    CmCoqProvider.prototype.getNext = function(prev) {

        var start = {line : 0, ch : 0};
        var doc = this.editor.getDoc();

        if (prev) {
            start = prev.end;
        }

        // EOF
        if (start.line === doc.lastLine() &&
            start.ch === doc.getLine(doc.lastLine()).length) {
            return null;
        }

        var token = this.getNextToken(start, /statementend|bullet|brace/);
        if (!token) return null;

        var end = {line : token.line, ch : token.end};

        for (var mark of doc.findMarks(end,end)) {
            mark.clear();
        }

        var stm = new CmSentence(start, end,
                                 doc.getRange({line : start.line, ch : start.ch},
                                              {line : token.line, ch : token.end}),
                                 token.type === 'comment'
                                );
        return stm;
    };

    // Gets sentence at point;
    CmCoqProvider.prototype.getAtPoint = function() {

        var doc   = this.editor.getDoc();
        var marks = doc.findMarksAt(doc.getCursor());

        // XXX
        if (marks.length) {
            return marks[0].stm;
        } else {
            return null
        }
        // } while(stm && (stm.end.line < cursor.line || stm.end.ch < cursor.ch));
    };

    // Mark a sentence with {clear, processing, error, ok}
    CmCoqProvider.prototype.mark = function(stm, mark) {

        var doc = this.editor.getDoc();

        switch (mark) {
        case "clear":
            stm.mark.clear();
            stm.mark = null;
            // XXX: Check this is the right place.
            // doc.setCursor(stm.start);
            break;
        case "processing":
            stm.mark = doc.markText(stm.start, stm.end, {className : 'coq-eval-pending'});
            stm.mark.stm = stm;
            break;
        case "error":
            stm.mark = doc.markText(stm.start, stm.end, {className : 'coq-eval-failed'});
            stm.mark.stm = stm;
            // XXX: Check this is the right place.
            doc.setCursor(stm.end);
            break;
        case "ok":
            stm.mark = doc.markText(stm.start, stm.end, {className : 'coq-eval-ok'});
            stm.mark.stm = stm;
            // XXX: Check this is the right place.
            // This interferes with the go to target.
            // doc.setCursor(stm.end);
            break;
        }
    };

    // If any marks, then call the invalidate callback!
    CmCoqProvider.prototype.onCMChange = function(evt) {

        var doc   = this.editor.getDoc();
        var marks = doc.findMarksAt(doc.getCursor());

        // We assume that the cursor is positioned in the change.
        if (marks.length === 1) {
            // XXX: Notify of the latest mark.
            this.onInvalidate(marks[0].stm);
        } else if (marks.length > 1) {
            console.log("Cursor in mark boundary, invalidating the first...");
            this.onInvalidate(marks[0].stm);
        }
    }

    // CM specific functions.

    // Returns the next token after the one seen at position: {line:…, ch:…}
    // type_re: regexp to match token type.
    // The returned object is a CodeMirror token with an additional attribute 'line'.
    CmCoqProvider.prototype.getNextToken = function(position, type_re) {
        var cm = this.editor;
        var linecount = cm.getDoc().lineCount();
        var token, next, ch, line;
        do {
            token = cm.getTokenAt(position);
            ch = token.end + 1;
            line = position.line;
            if (token.end === cm.getLine(line).length) {
                line++;
                ch = 0;
                if (line >= linecount)
                    return null;
            }
            next = cm.getTokenAt({line:line, ch:ch});
            next.line = line;
            position = {line:next.line, ch:next.end};
        } while(type_re && !(type_re.test(next.type)));
        return next;
    };

}());

// Local Variables:
// js-indent-level: 4
// End:
