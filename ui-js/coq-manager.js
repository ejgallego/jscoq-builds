// The CoqManager class.
// Copyright (C) 2015-2017 Mines ParisTech/ARMINES
//
// CoqManager manages a document composed of several coq snippets,
// allowing the user to send/retract indivual coq sentences throu
// them. The Coq snippets can be provided by several sources, we just
// require them to be able to list parts and implement marks.
//

// XXX: use RequireJS or something like that.
"use strict";

// Extra stuff:

Array.prototype.last    = function() { return this[this.length-1]; };
Array.prototype.flatten = function() { return [].concat.apply([], this); };

/***********************************************************************/
/* A Provider Container aggregates several containers, the main deal   */
/* here is keeping track of focus, as the focused container can be     */
/* different from the "active" one                                     */
/***********************************************************************/
class ProviderContainer {

    constructor(elms) {

        // Code snippets.
        this.snippets = [];

        // Debug variables
        var idx = 0;

        // for (e of elms) not very covenient here due to the closure.
        elms.forEach(function (e) {

            // Init.
            var cm = new CmCoqProvider(e);
            cm.idx = idx++;
            this.snippets.push(cm);

            // Track focus XXX (make generic)
            cm.editor.on('focus', evt => { this.currentFocus = cm; });

            // Track invalidate
            cm.onInvalidate = stm => { this.onInvalidate(stm); };
            cm.onMouseEnter = stm => { this.onMouseEnter(stm); };
            cm.onMouseLeave = stm => { this.onMouseLeave(stm); };

        },this);
    }

    // Get the next candidate and mark it.
    getNext(prev) {

        var spr, next;

        // If we have no previous element start with the first
        // snippet, else get the current one.
        if (!prev) {
            spr  = this.snippets[0];
            next = spr.getNext(null);
        } else {
            spr  = prev.sp;
            next = spr.getNext(prev);
        }

        // We got a snippet!
        if (next) {
            next.sp = spr;
            return next;
        } else {
            // Try the next snippet.
            var idx = this.snippets.indexOf(spr);
            while (idx < this.snippets.length - 1) {
                spr  = this.snippets[idx+1];
                next = spr.getNext(null);
                if (next) {
                    next.sp = spr;
                    return next;
                } else {
                    idx = this.snippets.indexOf(spr);
                }
            } // while
            // No next snippet :( !
            return null;
        }
    }

    mark(stm, mark) {
        stm.sp.mark(stm, mark);
    }

    // Focus and movement-related operations.

    // Get the point of the current focused element.
    getAtPoint() {
        return this.currentFocus.getAtPoint();
    }

    // Indicates if stm is after the point.
    // XXX: Improve
    afterPoint(stm) {

        var idx_point = this.snippets.indexOf(this.currentFocus);
        var idx_cur   = this.snippets.indexOf(stm.sp);

        return (idx_point < idx_cur);

    }

    cursorToStart(stm) {
        stm.sp.cursorToStart(stm);
    }

    cursorToEnd(stm) {
        stm.sp.cursorToEnd(stm);
    }

    focus() {
        if (this.currentFocus)
            this.currentFocus.focus();
        else
            this.snippets[0].focus();
    }

}

/***********************************************************************/
/* CoqManager coordinates the coq code objects, the panel, and the coq */
/* js object.                                                          */
/*                                                                     */
/***********************************************************************/

var copyOptions = function(obj, target) {
    if (!target) target = {};
    for (var prop in target) {
        if (obj.hasOwnProperty(prop)) {
            target[prop] = obj[prop];
        }
    }
    return target;
}

class CoqManager {

    constructor(elems, options) {

        options = options ? options : {};

        // Default options
        this.options = {
            prelude: true,
            debug:   false,
            wrapper_id: 'ide-wrapper',
            base_path:  "./",
            implicit_libs: false,
            init_pkgs: ['init'],
            all_pkgs:  ['init', 'math-comp',
                        'coq-base', 'coq-arith', 'coq-reals',
                        'coquelicot', 'flocq', 'sf', 'cpdt', 'color' ]
            // Disabled on 8.6
            // 'coquelicot', 'flocq', 'tlc', 'sf', 'cpdt', 'color', 'relalg', 'unimath',
            // 'plugin-utils', 'extlib', 'mirrorcore']
        };

        this.options = copyOptions(options, this.options);

        // Setup the Coq statement provider.
        this.provider = this.setupProvider(elems);

        // Setup the Panel UI.
        this.layout = new CoqLayoutClassic(this.options);

        // Setup the Coq worker.
        this.coq           = new Worker(this.options.base_path + 'coq-js/jscoq_worker.js');
        this.coq.onmessage = evt => this.coq_handler(evt);

        this.coq.sendCommand = msg => {
            if(this.options.debug) {
                console.log("Posting: ", msg);
            }
            this.coq.postMessage(msg);
        };

        // Keybindings setup
        // XXX: This should go in the panel init.
        document.addEventListener('keydown', evt => this.keyHandler(evt));

        // XXX: Depends on layout IDs.
        document.getElementById('hide-panel')
            .addEventListener('click', evt => this.layout.toggle() );

        // Panel setup 2: packages panel.
        // XXX: In the future this may also manage the downloads.
        this.packages = new PackageManager(this.layout.packages, this.options.base_path, this.coq);

        // Info
        this.packages.pkg_info = [];

        // Packages to load
        this.packages.pkg_init = [];

        // Pre-init packages
        this.pre_packages = [];

        // Display packages panel:
        var pkg_panel = document.getElementById('packages-panel').parentNode;
        pkg_panel.classList.remove('collapsed');
        this.layout.show();

        // Get Coq version, etc...
        this.coq.sendCommand(["GetInfo"]);

        // This is a sid-based index of processed statements.
        this.doc = {
            number_adds:        0,
            sentences:         [],
            stm_id:            [],
            goals:             []
        };

        this.error = [];

        // XXX: Initial sentence == hack
        let  dummyProvider = { mark : function() {},
                               getNext: function() { return null; },
                               focus: function() { return null; }
                             };
        this.doc.stm_id[1] = { text: "dummy sentence", coq_sid: 1, sp: dummyProvider };
        this.doc.sentences = [this.doc.stm_id[1]];

        // XXX: Hack
        this.waitForPkgs = [];

        // The fun starts: Load the set of packages.
        let bp = this.options.base_path + "../coq-pkgs/";
        this.coq.sendCommand(["InfoPkg", bp, this.options.all_pkgs]);
    }

    // Provider setup
    setupProvider(elems) {

        var provider = new ProviderContainer(elems);

        provider.onInvalidate = stm => {

            // If we have an error mark we need to clear it.
            let stm_err_idx = this.error.indexOf(stm);

            if (stm_err_idx >= 0) {
                provider.mark(stm, "clear");
                this.error.splice(stm_err_idx, 1);
                return;
            }

            this.goCursor();
        };

        provider.onMouseEnter = stm => {
            if (stm.coq_sid && this.doc.goals[stm.coq_sid])
                this.layout.update_goals(this.doc.goals[stm.coq_sid]);
        };

        provider.onMouseLeave = stm => {
            this.layout.update_goals(this.doc.goals[this.doc.sentences.last().coq_sid]);
        };

        return provider;
    }

    // Feedback Processing
    feedProcessingIn(sid) {
    }

    feedFileDependency(sid) {
    }

    feedFileLoaded(sid, file, mod) {
        this.layout.log(file + ' loading.', 'Info');
    }

    // The first state is ready.
    feedProcessed(sid) {

        this.layout.proof.textContent +=
            "\ncoq worker is ready with sid!! " + sid.toString() +
            "\nPlease, wait for library loading";

        this.feedProcessed = this.feedProcessedReady;
        this.enable();
    }

    feedProcessedReady(nsid) {

        if(this.options.debug)
            console.log('State processed', nsid);

        // The semantics of the stm here were a bit inconvenient: it
        // would send `ProcessedReady` feedback message before we the
        // `Stm.add` call has returned, thus we were not ready to
        // handle as we didn't know of their existance yet. The
        // typical example is when `Stm.add` forces an observe due to
        // side-effects.
        //
        // The new approach avoids this, but we ignore such feedback
        // just in case.

        var stm = this.doc.stm_id[nsid];

        if (!stm) {
            console.log('ready but cancelled user side?', nsid);
            return;
        }

        stm.executed = true;
        this.provider.mark(stm, "clear");
        this.provider.mark(stm, "ok");

        // Get goals
        this.coq.sendCommand(["Goals", nsid]);
    }

    // Simplifier to the "rich" format coq uses.
    richpp2HTML(msg) {

        // Elements are ...
        if (msg.constructor !== Array) {
            return msg;
        }

        var ret;
        var tag, ct, id, att, m;
        [tag, ct] = msg;

        switch (tag) {

        // Element(tag_of_element, att (single string), list of xml)
        case "Element":
            [id, att, m] = ct;
            let imm = m.map(this.richpp2HTML, this);
            ret = "".concat(...imm);
            ret = `<span class="${id}">` + ret + `</span>`;
            break;

        // PCData contains a string
        case "PCData":
            ret = ct;
            break;

        default:
            ret = msg;
        }
        return ret;
    }

    pp2HTML(msg) {

        // Elements are ...
        if (msg.constructor !== Array) {
            return msg;
        }

        var ret;
        var tag, ct;
        [tag, ct] = msg;

        switch (tag) {

        // Element(tag_of_element, att (single string), list of xml)
        case "Pp_glue":
            let imm = ct.map(this.pp2HTML, this);
            ret = "".concat(...imm);
            // ret = `<span class="${id}">` + ret + `</span>`;
            break;

        // PCData contains a string
        case "Pp_string":
            ret = ct;
            break;

        // Pp_box is encoded as an array of 3.
        case "Pp_box":
            var vmode = this.breakMode || false;

            switch(msg[1][0]) {
            case "Pp_vbox":
                this.breakMode = true;
                break;
            default:
                this.breakMode = false;
            }

            ret = this.pp2HTML(msg[2]);
            this.breakMode = vmode;
            break;

        // Pp_tag is encoded as an array of 3.
        case "Pp_tag":
            ret = this.pp2HTML(msg[2]);
            ret = `<span class="${msg[1]}">` + ret + `</span>`;
            break;

        case "Pp_force_newline":
            ret = "</br>";
            break;

        case "Pp_print_break":
            if (this.breakMode) {
                ret = "</br>";
            } else if (msg[2] > 0) {
                ret = "</br>";
            } else {
                ret = " ";
            }
            break;

        default:
            ret = msg;
        }
        return ret;
    }

    // Error handler.
    handleError(sid, loc, msg) {

        let err_stm;

        err_stm = this.doc.stm_id[sid];

        // The sentence has already vanished! This can happen for
        // instance if the execution of an erroneous sentence is
        // queued twice, which is hard to avoid due to STM exec
        // forcing on parsing.

        if(!err_stm) return;

        this.layout.log(msg, 'Error');

        // this.error will make the cancel handler to mark the stm red
        // instead of just clearing the mark.
        this.error.push(err_stm);

        let stm_idx       = this.doc.sentences.indexOf(err_stm);

        // The stm was not deleted!
        if (stm_idx >= 0) {
            this.doc.sentences.splice(stm_idx, 1);

            this.doc.stm_id[sid] = null;
            this.doc.goals[sid]  = null;
            err_stm.coq_sid = null;

            this.provider.mark(err_stm, "clear");
            this.provider.mark(err_stm, "error");

            this.coq.sendCommand(['Cancel', sid]);
        }
    }

    feedMessage(sid, lvl, loc, msg) {

        // var fmsg = this.richpp2HTML(msg);
        // var fsmg = msg.toString();
        var fmsg = this.pp2HTML(msg);

        // JSON encoding...
        lvl = lvl[0];

        if(this.options.debug)
            console.log('Message', sid, lvl, fmsg);

        // XXX: highlight error location.
        if (lvl === 'Error') {
            this.handleError(sid, loc, fmsg);
        } else {
            this.layout.log(fmsg, lvl);
        }
    }

    // Coq Message processing.
    coqFeedback(fb) {

        var feed_tag = fb.contents[0];

        if(this.options.debug)
            console.log('Feedback', fb.id, fb.contents);

        if( this['feed'+feed_tag] ) {
            fb.contents[0] = fb.id;
            this['feed'+feed_tag].apply(this, fb.contents);
        } else {
            console.log('Feedback type', feed_tag, 'not handled');
        }

    }

    coqAdded(nsid,loc) {

        if(this.options.debug)
            console.log('adding: ', nsid, loc);

        // XXX Rewrite, the sentence could have vanished...
        let cur_stm = this.doc.stm_id[nsid];

        if (this.goTarget) {
            // [Modulo the same old bugs, we need a position comparison op]
            if (this.provider.getAtPoint() || this.provider.afterPoint(cur_stm) ) {
            // if (this.provider.getAtPoint()) {

                // We have reached the destination...
                if(!cur_stm.executed) {
                    this.coq.sendCommand(['Exec', nsid]);
                }

                this.goTarget = false;
            } else {
                // We have not reached the destination, continue forward.
                this.goNext(false);
            }
        } else {
            if(!cur_stm.executed) {
                this.coq.sendCommand(['Exec', nsid]);
            }
        }
    }

    // Gets a list of cancelled sids.
    coqCancelled(sids) {

        if(this.options.debug)
            console.log('cancelling', sids);

        sids.forEach(function (sid) {

            let stm_to_cancel = this.doc.stm_id[sid];
            let stm_err_idx   = this.error.indexOf(stm_to_cancel);

            if (stm_err_idx >= 0) {

            } else {
                let stm_idx = this.doc.sentences.indexOf(stm_to_cancel);

                // Not already cancelled.
                if (stm_idx >= 0) {

                    this.doc.stm_id[sid] = null;
                    this.doc.goals[sid]  = null;
                    stm_to_cancel.coq_sid = null;

                    this.doc.sentences.splice(stm_idx, 1);

                    this.provider.mark(stm_to_cancel, "clear");
                }
            }

        }, this);

        // Update goals

        this.layout.update_goals(this.doc.goals[this.doc.sentences.last().coq_sid]);
    }

    coqGoalInfo(sid, goals) {

        var hgoals = this.pp2HTML(goals);
        this.doc.goals[sid] = hgoals;

        // XXX optimize!
        // if(!this.goTarget)
        this.layout.update_goals(hgoals);

    }

    coqLog(level, msg) {

        let rmsg = this.pp2HTML(msg);

        if (this.options.debug)
            console.log(rmsg, level[0]);

        this.layout.log(rmsg, level[0]);
    }

    coqLibInfo(bname, bi) {

        this.packages.addBundleInfo(bname, bi);
        this.packages.pkg_info[bname] = bi;

        // Check if we want to load this package.
        var rem_pkg = this.options.init_pkgs;
        var idx = rem_pkg.indexOf(bname);

        // Worker path is coq-js.
        let bp = this.options.base_path + "../coq-pkgs/";

        if(idx > -1) {
            this.coq.sendCommand(['LoadPkg', bp, bname]);
        }
    }

    coqLibProgress(evt) {
        this.packages.onPkgProgress(evt);
    }

    coqLibLoaded(bname) {

        this.packages.onBundleLoad(bname);

        var rem_pkg = this.options.init_pkgs;
        var idx = rem_pkg.indexOf(bname);

        if(idx > -1) {

            this.packages.pkg_init.push(bname);
            rem_pkg.splice(idx, 1);

            // All the packages have been loaded.
            if (rem_pkg.length === 0)
                this.coqInit();
        }
    }

    coqCoqExn(msg) {
        // this.layout.log(msg, "Error");
        console.log('coqExn', msg);
    }

    coqJsonExn(msg) {
        // this.layout.log(msg, "Error");
        console.log('jsonExn', msg);
    }

    coqCoqInfo(info) {

        this.layout.proof.textContent =
               info
            + "\nPlease wait for the libraries to load, thanks!"
            + "\nIf you have trouble try cleaning your browser's cache.\n";
    }

    // Coq Init: At this point, the required libraries are loaded
    // and Coq is ready to be used.
    coqInit() {

        // Hide the packages panel.
        var pkg_panel = document.getElementById('packages-panel').parentNode;
        pkg_panel.classList.add('collapsed');

        this.layout.proof.textContent +=
            "\n===> JsCoq filesystem initalized with success!\n" +
            "===> Loaded packages [" + this.options.init_pkgs.join(', ') + "] \n";

        // XXXXXX: Critical point
        var load_lib = [];
        var init_lib = this.options.init_pkgs;

        if (this.options.prelude) {
            load_lib.push(["Coq", "Init", "Prelude"]);
        }

        let bp = this.options.base_path + "../";

        let load_paths = this.packages.pkg_init.map(
            bundle => this.packages.pkg_info[bundle].pkgs
        ).flatten().map( pkg => pkg.pkg_id );

        this.coq.sendCommand(["Init", this.options.implicit_libs, load_lib, load_paths]);
        // Done!
    }

    coq_handler(evt) {

        var msg     = evt.data;
        var msg_tag = msg[0];

        if(this.options.debug)
            console.log("coq_evt", msg);

        // We call the corresponding coq$msg_tag(msg[1]..msg[n])

        if( this['coq'+msg_tag] ) {
            msg.shift();
            this['coq'+msg_tag].apply(this, msg);
        } else {
            console.log('Message type', msg_tag, 'not handled');
        }

    }

    goPrev() {

        // If we didn't load the prelude, prevent unloading it to
        // workaround a bug in Coq.
        if (this.doc.sentences.length <= 1) return;

        debugger;
        // XXX: Optimization, in case of error, but incorrect in the
        // new general framework.
        if (this.error.length > 0) {
            this.provider.mark(this.error.pop(), "clear");
            return;
        }

        var cur_stm  = this.doc.sentences.last();
        var prev_stm = this.doc.sentences[this.doc.sentences.length - 2];

        let update_focus = true;
        if(update_focus && prev_stm) {
            this.currentFocus = prev_stm.sp;
            this.currentFocus.focus();
            this.provider.cursorToStart(cur_stm);
        }

        // Cancel the sentence
        let stm_idx       = this.doc.sentences.indexOf(cur_stm);
        this.doc.sentences.splice(stm_idx, 1);

        this.doc.stm_id[cur_stm.coq_sid] = null;
        this.doc.goals[cur_stm.coq_sid]  = null;
        this.provider.mark(cur_stm, "clear");
        this.coq.sendCommand(['Cancel', cur_stm.coq_sid]);
        cur_stm.coq_sid = null;
    }

    // Return if we had success.
    goNext(update_focus) {

        let cur_stm = this.doc.sentences.last();
        let cur_sid = cur_stm.coq_sid;

        let next_stm = this.provider.getNext(cur_stm);

        // We are the the end
        if(!next_stm) { return false; }

        let next_sid = cur_sid+1;
        next_stm.coq_sid = next_sid;
        next_stm.executed = false;

        this.doc.sentences.push(next_stm);
        this.doc.stm_id[next_sid] = next_stm;

        // XXX: Hack to avoid sending comments. Is this still valid?
        if(next_stm.is_comment) {
            this.provider.mark(next_stm, "ok");
            return true;
        } else {
            this.provider.mark(next_stm, "processing");
        }

        // We focus the new snippet.
        if(update_focus) {
            this.currentFocus = next_stm.sp;
            this.currentFocus.focus();
            this.provider.cursorToEnd(next_stm);
        }

        // process special jscoq commands, for now:
        // Comment "pkg: list" will load packages.
        this.process_special(next_stm.text);
        this.coq.sendCommand(["Add", cur_sid, next_sid, next_stm.text]);

        // Avoid stack overflows by doing a commit every 24
        // sentences, due to the STM co-tail recursive traversal bug?
        let so_threshold = 24;
        if( (this.doc.number_adds++ % so_threshold) === 0 )
            this.coq.sendCommand(['Exec', next_sid]);

        return false;
    }

    goCursor() {

        var cur = this.provider.getAtPoint();

        if (cur) {
            if(!cur.coq_sid) {
                console.log("critical error, stm not registered");
            } else {
                this.coq.sendCommand(['Cancel', cur.coq_sid]);
            }
        } else {
            this.goTarget = true;
            this.goNext(false);
        }
    }

    // Drops all the state!
    reset() {

        // Reset out sentences
        this.doc.sentences.forEach(function(stm) {
            this.provider.mark(stm, "clear");
        }, this);

        // this.doc.sentences = [];
        // XXX Kill worker
    }

    // Keyboard handling
    keyHandler(e) {

        // All our keybindings are prefixed by alt.
        if (e.keyCode === 119) // F8
            this.layout.toggle();

        if (!e.altKey && !e.metaKey) return true;
        var btn_name;
        switch (e.keyCode) {
            case 13: // ENTER
                btn_name = 'to-cursor';
                break;
            case 78: // N
            case 40: // flèche en bas
                btn_name = 'down';
                break;
            case 80: // P
            case 38: // flèche en haut
                btn_name = 'up';
                break;
        }

        if(btn_name) {
            this.provider.focus();
            this.raiseButton(btn_name);
            e.preventDefault();
        }
    }

    // Enable the IDE.
    enable() {

        // XXX: Should be the task of the layout.
        this.btnEventHandler = this.toolbarClickHandler.bind(this);
        this.layout.buttons.addEventListener('click', this.btnEventHandler);
        this.layout.buttons.style.display = 'inline-block';
        this.layout.buttons.style.opacity = 1;
        this.provider.focus();
    }

    // Disable the IDE.
    disable() {

        // Disable the buttons.
        this.layout.buttons.removeEventListener('click', this.btnEventHandler);
        this.layout.buttons.style.display = 'none';
        this.layout.buttons.style.opacity = 0;
        this.layout.proof.textContent +=
                "\n===> Waiting for Package load!\n";
    }

    toolbarClickHandler(evt) {

        this.provider.focus();

        switch (evt.target.name) {
        case 'to-cursor' :
            this.goCursor();
            break;

        case 'up' :
            this.goPrev();
            break;

        case 'down' :
            this.goNext(true);
            break;
        }
    }

    raiseButton(btn_name) {

        // XXX: EG: Here I disagree with the current code, it
        // should be possible to use the coq manager without a toolbar!

        // This is a bit different from most UI indeed.
        var btns = this.layout.buttons.getElementsByTagName('img');
        var btn  = btns.namedItem(btn_name);

        if (btn) {
            btn.dispatchEvent(new MouseEvent('click',
                                             {'view'       : window,
                                              'bubbles'    : true,
                                              'cancelable' : true
                                             }));
        }
    }

    process_special(text) {

        var special;

        if (special = text.match(/Comments \"(.*): (.+)\"./)) {
            let cmd  = special[1];
            let args = special[2];

            switch (cmd) {

            case 'pkgs':
                let pkgs = args.split(' ');
                console.log('Requested pkgs '); console.log(pkgs);

                let pkg_panel = document.getElementById('packages-panel').parentNode;
                pkg_panel.classList.remove('collapsed');

                this.disable();
                this.waitForPkgs = pkgs;

                pkgs.forEach(this.coq.add_pkg, this);

                return true;

            default:
                console.log("Unrecognized jscoq command");
                return false;
            }
        }
        return false;
    }
}

// Local Variables:
// js-indent-level: 4
// End:
