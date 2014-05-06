
annotorious.plugin.DenichePlugin = function(opt_config_options) { 
	/** @private **/
	this._tags = [];
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

annotorious.plugin.DenichePlugin.prototype.addAnnotation = function (annotation) {
	// console.log('DenichePlugin.addAnnotation()');
	this.toggleButtons(annotorious.plugin.DenichePlugin.states.SOME);
	if (this._tags[annotation.targetId]) {	
		var old = this._tags[annotation.targetId];
		annotation.compound_text = old.compound_text;
		annotation.compound_length = annotation.compound_text.push(annotation.text);
		annotation.text = annotation.compound_text.join(', ');
		this._anno.addAnnotation(annotation, old);
		this._tags[annotation.targetId] = annotation;
		// console.log('tag replaced ' + old.text + ' by ', annotation.text);
	} else {
		annotation.compound_length = 1;
		annotation.compound_text = [ annotation.text ];
		this._anno.addAnnotation(annotation);
		this._tags[annotation.targetId] = annotation;
		// console.log('new tag added');
	}
	// console.log(this._anno.getAnnotations());
}

annotorious.plugin.DenichePlugin.prototype.removeAnnotation = function (label, targetId) {
	var old = this._tags[targetId];
	// console.log('removeAnnotation');
	if (old) {
		this._anno.removeAnnotation(old);
		var index = old.compound_text.indexOf(label);
		if (index > -1) old.compound_text.splice(index, 1);
		old.text = old.compound_text.join('; ');
		// console.log(old.text);
		this._anno.addAnnotation(old);
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
		oSelf._anno._deniche.currentShape = ev.shape;
	});

	this._anno.addHandler('onAnnotationCreated', function(ev) { 
		console.log('onAnnotationCreated', ev) 
	});
	this._anno.addHandler('onPopupShown', function(ev) { 
		console.log('onPopupShown', ev) 
		if (!ev.text) { 
			console.log("no text");

		}
	});

	this._anno.addHandler('onEditorShown', function(annotation) {
		if (annotation && annotation.shapes) {
			oSelf._anno._deniche.currentShape = annotation.shapes[0];
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
anno.addHandler('beforeAnnotationRemoved', function(ev) { console.log('beforeAnnotationRemoved', ev) });
anno.addHandler('onAnnotationRemoved', function(ev) { console.log('onAnnotationRemoved', ev) });
anno.addHandler('onAnnotationUpdated', function(ev) { console.log('onAnnotationUpdated', ev) });
