"use strict";

class PackageManager {

    /**
     * Creates the packages UI and loading manager.
     *
     * @param {Element} panel_dom <div> element to hold the package entries
     * @param {string} pkg_root_path base URL of package locations (coq-pkgs)
     * @param {list} pkg_names list of package names
     * @param {CoqWorker} coq reference to the Coq worker instance to send
     *   load requests to
     */
    constructor(panel_dom, pkg_root_path, pkg_names, coq) {
        this.pkg_root_path = pkg_root_path;
        this.pkg_names     = pkg_names;
        this.panel         = panel_dom;
        this.bundles       = {};
        this.loaded_pkgs   = [];
        this.coq           = coq;
    }

    addBundleInfo(bname, pkg_info) {

        var div  = document.createElement('div');
        var row = $(div);

        row.attr('data-name', bname);
        row.data('info', pkg_info);

        row.append($('<button>').addClass('download-icon')
                   .click(() => { this.startPackageDownload(pkg_info.desc); }));

        row.append($('<span>').text(pkg_info.desc));

        // Find bundle's proper place in the order among existing entries
        var place_before = null, idx = this.pkg_names.indexOf(bname);

        if (idx > -1) {
            for (let e of $(this.panel).children()) {
                let eidx = this.pkg_names.indexOf($(e).attr('data-name'));
                if (eidx == -1 || eidx > idx) {
                    place_before = e;
                    break;
                }
            }
        }

        this.panel.insertBefore(div, place_before /* null == at end */ );

        var desc = pkg_info.desc;
        var pkgs = pkg_info.pkgs;
        var no_files = 0;

        for(var i = 0; i < pkgs.length; i++)
            // pkgs[i].cma_files.length XXX
            no_files += pkgs[i].vo_files.length;

        pkg_info.loaded = 0;
        pkg_info.total  = no_files;

        this.bundles[bname] = { div: div, info: pkg_info };

        if (pkg_info.archive) {
            var archive = new CoqPkgArchive(this.getUrl(pkg_info.archive));
            archive.onProgress = evt => this.showPackageProgress(bname, evt);
            this.bundles[bname].archive = archive;
        }

        this.dispatchEvent(new Event('change'));
    }

    addBundleZip(bname, zip, pkg_info) {
        pkg_info = pkg_info || {};

        var archive = new CoqPkgArchive(zip);

        return archive.getPackageInfo().then(pi => {
            for (let k in pi)
                if (!pkg_info[k]) pkg_info[k] = pi[k];

            this.addBundleInfo(bname, pkg_info);
            this.bundles[bname].archive = archive;
        });
    }

    waitFor(init_pkgs) {
        return new Promise((resolve, reject) => {
            var observe = () => {
                if (init_pkgs.every(x => this.bundles[x])) {
                    this.removeEventListener('change', observe);
                    resolve(init_pkgs.map(x => this.bundles[x]));
                    return true;
                }
            };
            if (!observe())
                this.addEventListener('change', observe);
        });
    }

    searchBundleInfo(prefix, module_name, exact=false) {
        // Look for a .vo file matching the given prefix and module name
        var suffix = module_name.slice(0, -1),
            basename = module_name.slice(-1)[0],
            possible_filenames = ['.vo', '.vio'].map(x => basename + x);

        let startsWith = (arr, prefix) => arr.slice(0, prefix.length).equals(prefix);
        let endsWith = (arr, suffix) => suffix.length == 0 || arr.slice(-suffix.length).equals(suffix);

        let pkg_matches = exact ? pkg_id => pkg_id.equals(suffix)
                                : pkg_id => startsWith(pkg_id, prefix) &&
                                            endsWith(pkg_id, suffix);

        for (let bundle_key in this.bundles) {
            let bundle = this.bundles[bundle_key];
            for (let pkg of bundle.info.pkgs) {
                if (pkg_matches(pkg.pkg_id) &&
                    pkg.vo_files.some(entry => possible_filenames.indexOf(entry[0]) > -1))
                    return { pkg: bundle_key,
                             info: bundle.info, 
                             module: pkg.pkg_id.concat([basename]) };
            }
        }
    }

    searchModule(prefix, module_name, exact=false) {
        var binfo = this.searchBundleInfo(prefix, module_name, exact),
            lookupDeps = (binfo, key) =>
                (binfo && binfo.info.modDeps && binfo.info.modDeps.hasOwnProperty(key))
                    ? binfo.info.modDeps[key] : [];
        if (binfo) {
            var pkgs = new Set([binfo.pkg]),
                module_deps = lookupDeps(binfo, binfo.module.join('.'));
            this._scan(module_deps,
                m => {
                    var binfo = this.searchBundleInfo([], m.split('.'), true);
                    if (binfo) pkgs.add(binfo.pkg);
                    return lookupDeps(binfo, m);
                });
            binfo.deps = [...pkgs.values()];
        }
        return binfo;
    }

    getUrl(resource) {
        // Generate URL with the pkg_root_path as the base
        return new URL(resource,
                  new URL(this.pkg_root_path, PackageManager.scriptUrl))
    }

    getLoadPath() {
        return this.loaded_pkgs.map( bname => {
            let bundle = this.bundles[bname],
                phys = bundle.archive ? ['/lib'] : [];
            return bundle.info.pkgs.map( pkg => [pkg.pkg_id, phys] );
        }).flatten();
    }

    /**
     * Loads a package from the preconfigured path.
     * @param {string} pkg_name name of package (e.g., 'init', 'math-comp')
     */
    startPackageDownload(pkg_name) {
        var bundle = this.bundles[pkg_name], promise;

        if (bundle) {
            if (bundle.promise) return bundle.promise; /* load issued already */

            if (bundle.archive) {
                promise = 
                    Promise.all([this.loadDeps(bundle.info.deps),
                                 bundle.archive.unpack(this.coq)])
                    .then(() => this.onBundleLoad(pkg_name));
            }
            else {
                promise = 
                    Promise.all([this.loadDeps(bundle.info.deps),
                                 this.loadPkg(pkg_name)]);
            }

            bundle.promise = promise;
            return promise;
        }
        else {
            return Promise.reject(`bundle missing: ${pkg_name}`);
        }
    }

    /**
     * Updates the download progress bar on the UI.
     * @param {string} bname package bundle name
     * @param {object} info {loaded: <number>, total: <number>}
     */
    showPackageProgress(bname, info) {
        var bundle = this.bundles[bname];

        if (!bundle.bar) {
            // Add the progress bar if it does not exist already
            bundle.bar = $('<div>').addClass('progressbar');
            bundle.egg = $('<div>').addClass('progress-egg');

            bundle.bar.append(bundle.egg);
            $(bundle.div).append($('<div>').addClass('rel-pos').append(bundle.bar));
        }

        if (info) {
            var progress = info.loaded / info.total,
                angle    = (progress * 1500) % 360;
            bundle.egg.css('transform', `rotate(${angle}deg)`);
            bundle.bar.css('width', `${Math.min(1.0, progress) * 100}%`);
        }
    }

    /**
     * Marks the package download as complete, removing the progress bar.
     * @param {string} bname package bundle name
     */
    showPackageCompleted(bname) {
        var bundle = this.bundles[bname];

        $(bundle.div).find('.rel-pos').remove();
        $(bundle.div).find('button.download-icon').addClass('checked');
    }

    onBundleStart(bname) {
        this.showPackageProgress(bname);
    }

    onPkgProgress(evt) {
        var info = this.bundles[evt.bundle].info;
        ++info.loaded; // this is not actually the number of files loaded :\

        this.showPackageProgress(evt.bundle, info);
    }

    onBundleLoad(bname) {
        this.loaded_pkgs.push(bname);

        var bundle = this.bundles[bname];
        if (bundle._resolve) bundle._resolve();

        this.showPackageCompleted(bname);
    }

    loadDeps(deps) {
        return Promise.all(
            deps.map(pkg => this.startPackageDownload(pkg)));
    }

    loadPkg(pkg_name) {
        var bundle = this.bundles[pkg_name];

        return new Promise((resolve, reject) => {
            bundle._resolve = resolve 
            this.coq.loadPkg(this.pkg_root_path, pkg_name);
        });
    }

    collapse() {
        this.panel.parentNode.classList.add('collapsed');
    }

    expand() {
        this.panel.parentNode.classList.remove('collapsed');
    }

    /**
     * (auxiliary method) traverses a graph spanned by a list of roots
     * and an adjacency functor. Implements DFS.
     * @param {array} roots starting points
     * @param {function} adjacent_out u => array of successors
     */
    _scan(roots, adjacent_out) {
        var collect = new Set(),
            work = roots.slice();
        while (work.length) {
            var u = work.pop();
            if (!collect.has(u)) {
                collect.add(u);
                for (let v of adjacent_out(u)) work.push(v);
            }
        }
        return collect;
    }

    // No portable way to create EventTarget instances of our own yet;
    // hijack the panel DOM element :\
    dispatchEvent(evt)             { this.panel.dispatchEvent(evt); }
    addEventListener(type, cb)     { this.panel.addEventListener(type, cb); }
    removeEventListener(type, cb)  { this.panel.removeEventListener(type, cb); }
}


/**
 * Represents a bundle stored in a Zip archive; either a remote
 * file that has to be downloaded or a local one.
 */
class CoqPkgArchive {
    constructor(resource) {
        if (resource instanceof URL)
            this.url = resource;
        else if (resource.file /* JSZip-like */)
            this.zip = resource;
        else
            throw new Error(`invalid resource for archive: '${resource}'`);

        this.onProgress = () => {};
    }

    load() {
        return this.zip ? Promise.resolve(this) :
            this.download().then(data =>
                JSZip.loadAsync(data)).then(zip =>
                    { this.zip = zip; return this; });
    }

    download() {
        // Here comes some boilerplate
        return new Promise((resolve, reject) => {
            var xhr = new XMLHttpRequest();
            xhr.responseType = 'arraybuffer';
            xhr.onload = () => resolve(xhr.response);
            xhr.onprogress = (evt) => requestAnimationFrame(() => this.onProgress(evt));
            xhr.onerror = () => reject(new Error("download failed"));
            xhr.open('GET', this.url);
            xhr.send();
        });
    }

    readManifest() {
        var manifest = this.zip.file('coq-pkg.json');
        return manifest ?
                manifest.async('text').then(data => JSON.parse(data))
                .catch(err => {
                    console.warn(`malformed 'coq-pkg.json' in bundle ${this.url || ''} (${err})`);
                    return {}; 
                })
              : Promise.resolve({});
    }

    getPackageInfo() {
        return this.readManifest().then(pkg_info => {

            var entries_by_dir = {};

            this.zip.forEach((rel_path, entry) => {
                var mo = /^(?:(.*)[/])(.*[.](?:vo|vio|cm[ao]))$/.exec(rel_path);
                if (mo) {
                    var [, dir, fn] = mo;
                    (entries_by_dir[dir] = entries_by_dir[dir] || []).push(fn);
                }
            });

            var pkgs = [];
            for (let dir in entries_by_dir) {
                pkgs.push({
                    pkg_id: dir.split('/'),
                    vo_files: entries_by_dir[dir].map(x => [x])
                });
            }

            pkg_info.pkgs = pkgs;
            return pkg_info;
        });
    }

    async unpack(worker) {
        await this.load();

        var asyncs = [];
        this.zip.forEach((rel_path, entry) => {
            if (!entry.dir)
                asyncs.push(
                    entry.async('arraybuffer').then(content =>
                        worker.put(`/lib/${rel_path}`, content))
                );
        });
        return Promise.all(asyncs);
    }
}

// EG: This will be useful once we move file downloading to javascript.
// PackagesManager

class PackageDownloader {

    constructor(row, panel) {
        this.row = row;
        this.bar = null;
        this.egg = null;
        this.bundle_name = row.datum().desc;
        this.panel = panel;
        this.progress = 0; // percent
    }

    // This method setups the download handling.
    download() {

        // Allow re-download
        // this.row.select('img').on('click', null);

        this.bar = this.row.append('div')
            .attr('class', 'rel-pos')
            .append('div')
            .attr('class', 'progressbar');

        this.egg = this.bar
            .append('img')
            .attr('src', 'ui-images/egg.png')
            .attr('class', 'progress-egg');

        var files_total_length = 0;
        var files_loaded_cpt = 0;
        var pkgs = this.row.datum().pkgs;

        // Proceed to the main download.
        for(var i = 0; i < pkgs.length; i++)
            files_total_length += pkgs[i].no_files;

        document.body.addEventListener('pkgProgress',
            (evt) => {
                if(evt.detail.bundle_name === this.bundle_name) {
                    this.progress = ++files_loaded_cpt / files_total_length;
                    this.updateProgress();
                    if (files_loaded_cpt === files_total_length)
                        this.finishDownload();
                }
            }
        );

        jsCoq.add_pkg(this.bundle_name);

    }

    updateProgress() {
        var angle = (this.progress * 360 * 15) % 360;
        this.egg.style('transform', 'rotate(' + angle + 'deg)');
        this.bar.style('width', this.progress * 100 + '%');
    }

    finishDownload() {
        this.row.select('.rel-pos').remove();
        this.row.select('img')
            .attr('src', 'ui-images/checked.png');
    }
}


if (typeof document !== 'undefined' && document.currentScript)
    PackageManager.scriptUrl = new URL(document.currentScript.attributes.src.value, window.location);

// Local Variables:
// js-indent-level: 4
// End:
