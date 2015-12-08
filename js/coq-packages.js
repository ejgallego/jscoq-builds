var PackagesManager;

(function(){

    "use strict";

    PackagesManager = function(pkg_list, panel) {

        this.base_url = "coq-fs/";
        this.panel    = panel;

        // XXX: Use a promise.
        // Initialize from definitions_url
        var req = new XMLHttpRequest();
        req.open('GET', definitions_url);
        req.send(null);

        req.addEventListener('readystatechange',
                             () => {
                               if(req.readyState === 4 &&
                                    (req.status === 200 || req.status === 304 || req.status === 0))
                                    this.displayDefinitions(req);
                             });
    };

    // XXX: We are going to port this to angular or some UI thingy.
    PackagesManager.prototype.displayDefinitions = function(req) {

        // Add a div for each package.
        var rows = d3.select(this.panel).selectAll('div')
                     .data(JSON.parse(req.responseText))
                     .enter()
                     .append('div');

        // Build the package UI for each DIV:
        var self = this;
        rows.each(function (rdiv) {

            var row = d3.select(this);

            row.append('img')
               .attr('src', 'images/dl.png')
               .on('click', () => {self.downloadPackage();});

            row.append('span')
               .text(d => d.pkg_id.join('.'));
        });
    };

    // PackagesManager.prototype.sendCoqPkg = function() {

    // XXX: Use a closure to replace the parentNode.
    PackagesManager.prototype.downloadPackage = function() {
        var row = d3.select(d3.event.target.parentNode);
        var dl  = new PackageDowloader(this.base_url, row);
        dl.download();
    };

    var PackageDowloader = function(base_url, row) {
        this.base_url = base_url;
        this.row = row;
        this.bar = null;
        this.egg = null;

        var pkg_info = row.datum();
        var pkg_base_url = base_url + pkg_info.pkg_id.join('_') + '/';
        this.urls = [];

        // Add all the urls to the task list.
        pkg_info.vo_files.forEach(f => this.urls.push(pkg_base_url + f));
        pkg_info.cma_files.forEach(f => this.urls.push(pkg_base_url + f));

        // Init progress parameters.
        this.steps_cpt = this.urls.length;
        this.steps_done = 0;
        this.progress = 0; // percent
    };

    // Start the download.
    PackageDowloader.prototype.download = function() {
        this.row.select('img').on('click', null);
        this.bar = this.row.append('div')
            .attr('class', 'rel-pos')
            .append('div')
            .attr('class', 'progressbar');
        this.egg = this.bar
            .append('img')
            .attr('src', 'images/egg.png')
            .attr('class', 'progress-egg');

        // XXX: This should call the Coq method.
        this.downloadNext();
    };

    PackageDowloader.prototype.downloadNext = function() {

        var url = this.urls.shift();
        if(!url)
            return this.finishDownload();

        var req = new XMLHttpRequest();
        req.addEventListener('progress', evt => this.onFileDlProgress(evt));
        req.addEventListener('load',     evt => this.onFileDlFinished(evt));
        req.open('GET', url);
        req.send(null);
    };

    // Event called for a progress report.
    PackageDowloader.prototype.onFileDlProgress = function(evt) {
        //console.log(evt);
        this.progress = (this.steps_done / this.steps_cpt +
                         evt.loaded / evt.total / this.steps_cpt) * 100;
        this.updateProgress();
    };

    // Event called for a finished report.
    // Main event to call from Coq.
    PackageDowloader.prototype.onFileDlFinished = function(evt) {
        //console.log(evt);
        this.steps_done++;
        this.progress = this.steps_done / this.steps_cpt * 100;
        this.updateProgress();
        this.downloadNext();
    };

    // UI thingy
    PackageDowloader.prototype.updateProgress = function() {
        var angle = (this.progress / 100 * 360 * 15) % 360;
        this.egg.style('transform', 'rotate(' + angle + 'deg)');
        this.bar.style('width', this.progress + '%');
    };

    // Mark the download as done.
    PackageDowloader.prototype.finishDownload = function() {
        this.row.select('.rel-pos').remove();
        this.row.select('img')
            .attr('src', 'images/checked.png');
    };

}());

// Local Variables:
// js-indent-level: 4
// End:
