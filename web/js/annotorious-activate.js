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
		annotation.compound_text = old.compound_text;
		annotation.compound_length = annotation.compound_text.push(annotation.text);
		annotation.text = annotation.compound_text.join(', ');
		this._anno.addAnnotation(annotation, old);
		this._tags[annotation.targetId] = annotation;
		console.log('tag replaced ' + old.text + ' by ', annotation.text);
	} else {
		annotation.compound_length = 1;
		annotation.compound_text = [ annotation.text ];
		this._anno.addAnnotation(annotation);
		this._tags[annotation.targetId] = annotation;
		console.log('new tag added');
	}
}

annotorious.plugin.DenichePlugin.prototype.removeAnnotation = function (label, targetId) {
	var old = this._tags[targetId];
	console.log('removeAnnotation');
	if (old) {
		this._anno.removeAnnotation(old);
		var index = old.compound_text.indexOf(label);
		if (index > -1) old.compound_text.splice(index, 1);
		old.text = old.compound_text.join('; ');
		console.log(old.text);
		this._anno.addAnnotation(old);
	}
}

anno.addPlugin('DenichePlugin', {});
