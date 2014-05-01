annotorious.plugin.DenichePlugin = function(opt_config_options) { 
	/** @private **/
	this._tags = [];
}
annotorious.plugin.DenichePlugin.prototype.initPlugin = function(anno) { 
	anno._deniche = this;
	this._anno = anno;
}

annotorious.plugin.DenichePlugin.prototype.toggleButtons = function(state) {
	if (!this.cancelButton) return;
	if (state == 'done') {
		this.cancelButton.style.display="none";
		this.saveButton.style.display="inline-block";
	} else if (state == 'new') {
		this.saveButton.style.display="none";
		this.cancelButton.style.display="inline-block";
	}
}

annotorious.plugin.DenichePlugin.prototype.filterTags = function(annotation) {
	var oSelf = this;
	YUI().use('node', 'event', function(Y) {
		var zero_tags_show = true;
		Y.one('a.annotorious-editor-button-save').setHTML('Done'); // Hack: turn save into done button
		Y.all('li.tagitem').each(function(tagNode) {
			if (annotation && annotation.targetId == tagNode.getAttribute('targetId')) {
				tagNode.show();
				zero_tags_show = false;
			} else {
				tagNode.hide();
			}
		});
		if (zero_tags_show) {
			oSelf.toggleButtons('new');
		}
	});
}

annotorious.plugin.DenichePlugin.prototype.onInitAnnotator = function(annotator) {
	this._extendEditor(annotator); 
}

annotorious.plugin.DenichePlugin.prototype.addAnnotation = function (annotation) {
	this.toggleButtons('done');
	if (this._tags[annotation.targetId]) {	
		var old = this._tags[annotation.targetId];
		annotation.text = old.text + '; ' + annotation.text;
		this._anno.addAnnotation(annotation, old);
		this._tags[annotation.targetId] = annotation;
	} else {
		this._anno.addAnnotation(annotation);
		this._tags[annotation.targetId] = annotation;
	}
}

anno.addPlugin('DenichePlugin', {});
