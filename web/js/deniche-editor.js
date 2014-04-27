annotorious.plugin.DenichePlugin.prototype._extendEditor = function(annotator) {
	// console.log('_extendEditor');
	var self = this;
	var fieldsEl = document.getElementById('fields');
	this._annotator = annotator;
	annotator.editor.addField(fieldsEl);
	annotator.editor.addField(function(an2) { 
			console.log(an2) ; 
			return an2.id;
		});

	anno.addHandler('onSelectionCompleted', function(ev) {
		console.log('onSelectionCompleted');
		console.log(ev);
		anno.currentShape = ev.shape;
	});
	anno.addHandler('onEditorShown', function(annotation) {
		console.log('onEditorShown');
		console.log(annotation);
		if (annotation && annotation.shapes) {
			anno.currentShape = annotation.shapes[0];
			YUI().use('node', 'event', function(Y) {
				Y.all('li.tagitem').each(function(tagNode) {
					console.log(tagNode.getAttribute('hash'));
				})
			})
		}
	});
}


