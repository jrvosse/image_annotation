
annotorious.plugin.DenichePlugin = function(opt_config_options) { 
	/** @private **/
	this._cleantags = [];
	this._dirtytags = [];
	this.currentShape = null;
	this.currentTargetId = null;
}

annotorious.plugin.DenichePlugin.states = { EMPTY:'empty', SOME:'some' };

annotorious.plugin.DenichePlugin.prototype.initPlugin = function(anno) { 
	this._anno = anno;
	this._anno._deniche = this;
	this._state = annotorious.plugin.DenichePlugin.states.EMPTY;
}

annotorious.plugin.DenichePlugin.prototype.toggleButtons = function(state) {
	if (!this.cancelButton) return;
	if (!state) state = annotorious.plugin.DenichePlugin.states.SOME;
	if (state == annotorious.plugin.DenichePlugin.states.SOME) {
		this.cancelButton.style.display="none";
		this.saveButton.style.display="inline-block";
	} else if (state == annotorious.plugin.DenichePlugin.states.EMPTY) {
		this.saveButton.style.display="none";
		this.cancelButton.style.display="inline-block";
	}
}

annotorious.plugin.DenichePlugin.prototype.filterTags = function(annotation) {
	var oSelf = this;
	YUI().use('node', 'event', function(Y) {
		var zero_tags_show = true;
		Y.all('li.tagitem').each(function(tagNode) {
			if (annotation && annotation.targetId == tagNode.getAttribute('targetId')) {
				tagNode.show();
				zero_tags_show = false;
			} else {
				tagNode.hide();
			}
		});
		if (zero_tags_show) {
			oSelf.toggleButtons(annotorious.plugin.DenichePlugin.states.EMPTY);
		}
	});
}

annotorious.plugin.DenichePlugin.prototype.removeAnnotation = function (label, targetId) {
	// console.log('DenichePlugin.removeAnnotation');
	this.currentTargetId = targetId;
	var old = this._dirtytags[targetId];
	if (!old) old = this._cleantags[targetId];
	if (old) {	
		var annotation = JSON.parse(JSON.stringify(old)); // deep copy
		var index = old.compound_text.indexOf(label);
		annotation.compound_text = old.compound_text.splice(index, 1);
		annotation.text = old.compound_text.join(', ');
		this._dirtytags[targetId] = annotation;
		if (!annotation.text) this.toggleButtons(annotorious.plugin.DenichePlugin.states.EMPTY);
	}
	
}

annotorious.plugin.DenichePlugin.prototype.addAnnotation = function (annotation, update) {
	// console.log('DenichePlugin.addAnnotation', update);
	this.currentTargetId = annotation.targetId;
	this.toggleButtons(annotorious.plugin.DenichePlugin.states.SOME);
	var old = this._dirtytags[annotation.targetId];
	if (!old) old = this._cleantags[annotation.targetId];
	if (old) {	
		annotation.compound_text = old.compound_text;
		annotation.compound_text.push(annotation.text);
		annotation.text = annotation.compound_text.join(', ');
	} else {
		annotation.compound_text = [ annotation.text ];
	}

	if (update) {
		this._cleantags[annotation.targetId] = annotation;
		this._anno.addAnnotation(annotation, old);
	} else {
		this._dirtytags[annotation.targetId] = annotation;
	}
}

annotorious.plugin.DenichePlugin.prototype.flushDirtyAnnotation = function(original) {
		var dirty = this._dirtytags[this.currentTargetId];
		console.log('replacing ', original, dirty);
		if (dirty) { 
			this._anno.addAnnotation(dirty,original);
			this._cleantags[this.currentTargetId] = dirty;
			this._dirtytags[this.currentTargetId] = null;
		}
}

annotorious.plugin.DenichePlugin.prototype.onInitAnnotator = function(annotator) {
	this.saveButton   = document.getElementsByClassName('annotorious-editor-button-save').item(0); 
	this.cancelButton = document.getElementsByClassName('annotorious-editor-button-cancel').item(0); 
	this.saveButton.innerHTML = "Done";

	var fieldsEl = document.getElementById('fields');
	annotator.editor.addField(fieldsEl);

	var oSelf = this;
	this._anno.addHandler('onSelectionCompleted', function(ev) {
		oSelf.currentShape = ev.shape;
	});

	this._anno.addHandler('onAnnotationCreated', function(original) { 
		// console.log('onAnnotationCreated') 
		oSelf.flushDirtyAnnotation(original);
	});
	this._anno.addHandler('onAnnotationUpdated', function(original) { 
		// console.log('onAnnotationUpdated') 
		oSelf.flushDirtyAnnotation(original);
	});

	this._anno.addHandler('onEditorShown', function(annotation) {
		if (annotation && annotation.shapes) {
			oSelf.currentShape = annotation.shapes[0];
			oSelf.toggleButtons(annotorious.plugin.DenichePlugin.states.SOME);
			oSelf.filterTags(annotation);
		} else {
			oSelf.toggleButtons(annotorious.plugin.DenichePlugin.states.EMPTY);
			oSelf.filterTags(null);
		}
	});
}

anno.addPlugin('DenichePlugin', {});

// Debug:
// anno.addHandler('beforeAnnotationRemoved', function(ev) { console.log('beforeAnnotationRemoved', ev) });
// anno.addHandler('onAnnotationRemoved', function(ev) { console.log('onAnnotationRemoved', ev) });
// anno.addHandler('onAnnotationUpdated', function(ev) { console.log('onAnnotationUpdated', ev) });
