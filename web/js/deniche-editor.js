annotorious.plugin.DenichePlugin.prototype._extendEditor = function(annotator) {
	var self = this;
	var fieldsEl = document.getElementById('fields');
	this._annotator = annotator;
	var dl = document.getElementsByClassName('annotorious-popup-button-delete').item(0);
	dl.parentNode.removeChild(dl);
	annotator.editor.addField(fieldsEl);
	annotator.editor.addField(function(an2) { 
			// Debug only
			// return an2.targetId;
		});

	anno.addHandler('onSelectionCompleted', function(ev) {
		// HACK:
		// set shape when new selection is completed so we can use it in the YUI autocompletion plugins.
		anno.currentShape = ev.shape;
	});

	anno.addHandler('onEditorShown', function(annotation) {
		// HACK:
		// set shape when adding to existing annotation so we can use it in the YUI autocompletion plugins.
		if (annotation && annotation.shapes) {
			anno.currentShape = annotation.shapes[0];
		}
		YUI().use('node', 'event', function(Y) {
			Y.all('li.tagitem').each(function(tagNode) {
				if (annotation && annotation.targetId == tagNode.getAttribute('targetId'))
					tagNode.show();
				else
					tagNode.hide();

			})
		})
	});
}


