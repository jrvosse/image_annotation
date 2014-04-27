annotorious.plugin.DenichePlugin = function(opt_config_options) { 
	/** @private **/
	this._tags = [];
}
annotorious.plugin.DenichePlugin.prototype.initPlugin = function(anno) { 
	anno._deniche = this;
	this._anno = anno;
}

annotorious.plugin.DenichePlugin.prototype.onInitAnnotator = function(annotator) {
	this._extendEditor(annotator); 
}

annotorious.plugin.DenichePlugin.prototype.addAnnotation = function (annotation) {
	if (this._tags[annotation.targetId]) {	
		var old = this._tags[annotation.targetId];
		annotation.text = old.text + '; ' + annotation.text;
		this._anno.addAnnotation(annotation, old);
	} else {
		this._tags[annotation.targetId] = annotation;
		this._anno.addAnnotation(annotation);
	}
}

anno.addPlugin('DenichePlugin', {});
