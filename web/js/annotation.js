/*
 * Annotation class represents a single annotation field.
 * More complex interfaces can be built by combining multiple fields.
 * 
 */

YUI.add('annotation', function(Y) {
	function Annotation(config) {
		Annotation.superclass.constructor.apply(this, arguments);
	}
	Annotation.NAME = "aclist"; // use same name as Y.Plugin.AutoComplete to inherit css
	Annotation.NS = "annotation";
	Annotation.ATTRS = {
		target: 		{ value: null }, // URI of target image to be annotated
		field: 			{ value: null }, // URI identifying annotation field
		store: 			{ value: null }, // URIs of web services to CRUD http annotation api
		tags: 			{ value: [] },	 // tags already exisiting
		startTyping: 		{ value: [] },   // timestamp when users start typing
		commentEnabled: 	{ value: "false" },// when true comment field is shown for this field
		unsureEnabled: 		{ value: "true" }, // when true "I'm not sure" checkboxes will be shown for each tag
		agreeEnabled: 		{ value: "true" }, // when true "I agree" checkboxes will be shown for each tag
		disagreeEnabled:	{ value: "true" }, // when true "I disagree" checkboxes will be shown for each tag
		deleteCommentEnabled:	{ value: "true" }, // when true comment overlay is shown for deletions on this field
		commentNode: 		{ value: null }, // node that holds the comment field if enabled
		uiLabels: 		{ value: [] },   // dictionairy with ui labels in the prefered language of the user
		// disallowing this is not yet implemented:
		allowTextSubmit:	{ value:true }   // if true, plain tags are allowed, if false, only terms with a uri.
	};

	Annotation.LIST_CLASS = 'taglist';
	Annotation.LIST_TEMPLATE = '<ul class="'+Annotation.LIST_CLASS+'"></ul>';

	Y.extend(Annotation, Y.Plugin.AutoComplete, {

		initializer: function(args) {
			// convert string "true"/"false" to boolean true/false
			var unsureEnabled = this.get('unsureEnabled'); 	this.set('unsureEnabled', unsureEnabled == "true");
			var agreeEnabled = this.get('agreeEnabled'); 	this.set('agreeEnabled', agreeEnabled == "true");
			var disagreeEnabled = this.get('disagreeEnabled'); this.set('disagreeEnabled', disagreeEnabled == "true");
			var commentEnabled = this.get('commentEnabled'); this.set('commentEnabled', commentEnabled == "true");
			var deleteCommentEnabled = this.get('deleteCommentEnabled'); this.set('deleteCommentEnabled', deleteCommentEnabled == "true");

			this.tags = new Y.Recordset({records:{}});
			this.tags.on("add", this._addTags, this);
			this.tags.on("remove", this._removeTags, this);

			this.tagList = Y.Node.create(Annotation.LIST_TEMPLATE);

			var parentNode = this.DEF_PARENT_NODE;
			parentNode.append(this.tagList);
			this.infoNode = new Y.Overlay({}).render(parentNode);
			this._createCommentNode(parentNode);
			this.on("activeItemChange", this._onHover, this);
			this.on("hoveredItemChange", this._onHover, this);
			this.on("select", this._onItemSelect, this);
			if (deleteCommentEnabled) {
				Y.delegate("click", this._onTagRemoveClick, this.tagList, 'li .remove', this);
				this._createDeleteNode(parentNode);
			} else {
				Y.delegate("click", this._onDelete, this.tagList, 'li .remove', this);
			}

			Y.Global.on("done", this._onDone, this);
			var firstkey = true;
			this._setKeyInputHandler(firstkey);
			this.get("inputNode").on("key", this._onTextSubmit, "enter", this);
			this.getTags();
		},

		_setKeyInputHandler : function(first) {
			if (first) {
				this.get("inputNode").on("key", this._onFirstKey, 'press:', this);
				// this.get("inputNode").detach("key", this._onTextSubmit, 'enter');
			} else {
				this.get("inputNode").detach("key", this._onFirstKey, 'press:');
				// this.get("inputNode").on("key",	 this._onTextSubmit, 'enter', this);
			}

		},
		_onFirstKey : function(e) {
			if (e.button == 13) return; // ? not sure why I get the submit return here ...
			this.set("startTyping", new Date());
			var firstkey = false;
			this._setKeyInputHandler(firstkey);

		},
		_onDone : function(e) {
				  Y.log("done event!");
				  this._onTextSubmit({});
			  },
		_renderTags : function(tags, index) {
			var tagList = this.tagList;
			// format the tags
			for(var i=0; i < tags.length; i++) {
				tagList.append('<li>'+this.formatTag(tags[i])+'</li>');

			};
			this.DEF_PARENT_NODE.all('.unsureButton').detach('click').on(  'click', this.onJudgeAnnotation, this, 'unsure');
			this.DEF_PARENT_NODE.all('.agreeButton').detach('click').on(   'click', this.onJudgeAnnotation, this, 'agree');
			this.DEF_PARENT_NODE.all('.disagreeButton').detach('click').on('click', this.onJudgeAnnotation, this, 'disagree');

		},
		_getTag : function(annotation) {
			var tagList = this.tagList;
			for(var i=0; i < tags.length; i++) {
				tagList.append('<li>'+this.formatTag(tags[i])+'</li>');

			};
		},

		_addTags : function(o) {
			this._renderTags(o.added, o.index);
		},
		_removeTags : function(o) {
			var tagNodes = this.tagList.all("li"),
				index = o.index,
				range = o.range;
			for (var i=index; i < index+range; i++) {
				tagNodes.item(i).remove();
			}
		},

		formatTag : function(tag) {
			var label = tag.getValue("label");
			var body = tag.getValue("body");
			var comment = tag.getValue("comment");
			var link = tag.getValue("display_link");
			var judgement_buttons = '';
			if (this.get('agreeEnabled')) {
				var agreeLabel = this.get('uiLabels').agreeLabel;
				var agree_value = tag.getValue("agree");
				var checked = (agree_value != '')?'checked':'';
				judgement_buttons += "<span class='agreeButton " + checked + "'>[" + agreeLabel +"]</span>";
			} 
			if (this.get('unsureEnabled')) {
				var unsureLabel = this.get('uiLabels').unsureLabel;
				var unsure_value = tag.getValue("unsure");
				var checked = (unsure_value != '')?'checked':'';
				judgement_buttons += "<span class='unsureButton " + checked + "'>[" + unsureLabel +"]</span>";
			}
			if (this.get('disagreeEnabled')) {
				var disagreeLabel = this.get('uiLabels').disagreeLabel;
				var disagree_value = tag.getValue("disagree");
				var checked = (disagree_value != '')?'checked':'';
				judgement_buttons += "<span class='disagreeButton " + checked + "'>[" + disagreeLabel +"]</span>";
			} 
			html = '<div class="label">';
			if (link == '')
				html += judgement_buttons + label;
			else
				html += judgement_buttons + '<a href="'+link+'">'+label+'</a>';

			if (comment && comment != "") {
			  html += ' (' + comment +')';
			}
			html += '</div><div class="remove"><a href="javascript:{}">x</a></div>';
			return html;
		},

		_onTagRemoveClick : function(e) {
			var eList = this.tagList;
			Y.log(eList);
			var index = this.tagList.all("li").indexOf(e.currentTarget.get("parentNode")),
			    tags = this.tags,
			    record = tags.getRecordByIndex(index),
			    annotation = record.getValue("annotation"),
			    labels = this.get("uiLabels"),
			    tag = record.getValue("label"),
			    ov = this.deleteOverlay,
			    n = ov.get('srcNode');
			ov.set("headerContent", "<h3 class='delete_dialog'>"+ labels.deleteLabel + " " + tag +"</h3>");
			n.one('.delete-comment-input').detach();
			n.one('#confirm-delete').detach();
			n.one('#cancel-delete').detach();

			n.one('.delete-comment-input').on("key", this._onDelete, "enter", this, annotation, index);
			n.one('#confirm-delete').on("click", this._onDelete, this, annotation, index);
			n.one('#cancel-delete').on("click", this._onCancel, this);
			ov.show();


		},

		_onCancel : function() {
				Y.log('onCancel');
				Y.one('.delete-comment-input').detach("key", this._onDelete, "enter");
				Y.one('.delete-comment-input').set("value", "");
				Y.one('#confirm-delete').detach("click", this._onDelete);
				Y.one('#cancel-delete').detach("click", this._onCancel);
				this.deleteOverlay.hide();
			     },

		_onDelete : function (e, annotation, index) {
			var tags = this.tags;
			var ov = this.deleteOverlay;
			if (ov) {
			    var n = ov.get('srcNode');
		            var commentNode =  n.one('.delete-comment-input');
			    var comment = commentNode.get("value");
			    commentNode.set("value", "");
			    n.one('.delete-comment-input').detach("key", this._onDelete, "enter");
			    n.one('#confirm-delete').detach("click", this._onDelete);
			    n.one('#cancel-delete').detach("click", this._onCancel);
			    ov.hide();
			} else {
		            var comment = '';
			    var index = this.tagList.all("li").indexOf(e.currentTarget.get("parentNode"));
			    var record = tags.getRecordByIndex(index);
			    var annotation = record.getValue("annotation");
			}

			Y.log('remove annotation '+annotation+' with comment: '+comment);
			Y.io(this.get("store.remove"), {
				data:{ annotation:annotation, comment:comment },
				on:{success: function(e) { tags.remove(index) }
				}
			});
		},
		getTags : function() {
			    var target = this.get('target'),
				 field = this.get('field');
			    var oSelf = this;
			    Y.io(this.get("store.get"),
				 { data: {
					 target: target,
					 field:  field
					 },
				   on: {
				       success: function(e,o) {
						  var r = Y.JSON.parse(o.responseText);
						  if (r && r[field] && r[field].annotations) {
						    var data =  r[field].annotations;
						    oSelf.tags.add(data);
						  }
						}
				       }
				 }
				);
			  },
		_onHover : function(e) {
			var infoNode = this.infoNode,
				active = e.newVal,
				body = '';
			if(active && active.getData().result.raw.info) {
				Y.log(active.getData());
				var scope = active.getData().result.raw.info.scopeNotes[0];
				var defin = active.getData().result.raw.info.definitions[0];
				var alts =  active.getData().result.raw.info.altLabels;
				if (scope && scope.en) { body += "<div class='scope'>"+scope.en+"</div>"; }
				if (defin && defin.en) { body += "<div class='defin'>"+defin.en+"</div>"; }
				for (var i=0; i<alts.length; i++) {
					body += "<span class='altLabel'>" +alts[i] + "</span>" ;
				}
			}
			if(body) {
				infoNode.set("bodyContent", body);
				infoNode.set("align", {node:active,
				                      points:[Y.WidgetPositionAlign.TL, Y.WidgetPositionAlign.TR]});
				infoNode.show();
			} else {
				infoNode.hide();
			}
		},
		_onItemSelect : function(e) {
			Y.log('onItemSelect');
			var type = 'tag';
			if (e.preventDefault) e.preventDefault();
			var item = e.details[0].result.raw;
			var comm = this.getComment();
			this._setKeyInputHandler(true);
			var now = new Date();
			var delta = now - this.get("startTyping");
			var target = this.get("target");
			if (item.uri && item.label) {
			  this.submitAnnotation(type, target, {type:"uri", value:item.uri }, item.label, comm, delta);
			} else {
			  this.submitAnnotation(type, target, {type:"literal", value: item}, item,       comm, delta);
			};
			this.get("inputNode").set("value", "");
		},

		_onTextSubmit : function(e) {
			Y.log('onTextSubmit');
			Y.log(e);
			if (e.preventDefault) e.preventDefault();
			this._setKeyInputHandler(true);
			if(!this.get("activeItem")) {
			        var type = 'tag';
				var now = new Date();
				var delta = now - this.get("startTyping");
				var value = this.getTag();
				var comm = this.getComment();
				var target = this.get("target");
				this.submitAnnotation(type, target, {type:"literal", value:value}, value, comm, delta);
			}
		},

		getTag: function() {
			      var value = this.get("inputNode").get("value");
			      this.get("inputNode").set("value", "");
			      return value?value:'';
		},

		getComment: function() {
			      var commentNode = this.get("commentNode");
			      Y.log(commentNode);
			      if (!commentNode) return "";
			      var c = commentNode.get("value");
			      Y.log(c);
			      commentNode.set("value", "");
			      return c;
		},

	        onJudgeAnnotation : function (ev, value) {
		        var eLi = ev.currentTarget.get("parentNode").get("parentNode");
			var index = this.tagList.all("li").indexOf(eLi);
			var tags = this.tags;
			var record = tags.getRecordByIndex(index);
			var target = record.getValue("annotation");
			var type = "judgement";
			this.submitAnnotation(type, target, {type:"literal", value: value});
		},

		submitAnnotation : function(type, target, body, label, comment, timing) {
		        if (!target) return;
		        if (!body.value) return;
			if (!label) label = body.value;
			if (!comment) comment = '';
			if (!timing) timing = -1;
			if (!type) type = 'default';
			Y.log('add tag: '+body.value+' with label: '+label+ ', time: ' + timing);

			var inputNode = this.get("inputNode");
			var tags = this.tags;

			Y.io(this.get("store.add"), {
				data:{
					field:this.get("field"),
					target:target,
					body:Y.JSON.stringify(body),
					label:label,
					typing_time: timing,
					type: type,
					comment: comment
				},
				on:{success: function(e,o) {
					var r = Y.JSON.parse(o.responseText);
					if (type == "tag")
						tags.add({body:body, label:label, annotation:r.annotation, comment:comment, type:type, display_link:r.display_link});
					
					inputNode.focus();
				    }
				}
			});
		},

		_createDeleteNode : function(parentNode) {
			Node = new Y.Overlay({}).render(parentNode);
			Node.hide();
			var labels = this.get('uiLabels');
			var head = "";
			var body = "<div class='annotate-comment delete-comment'>";
			body += "<h3>" + labels.commentLabel + "</h3>";
			body += "<textarea class='annotate-comment-input delete-comment-input' />";
			var foot  = "<button id='cancel-delete'>" + labels.cancelDeleteLabel + "</button>";
			foot += "<button id='confirm-delete'>" +labels.confirmDeleteLabel+ "</button>";
			Node.set("headerContent", head);
			Node.set("bodyContent",   body);
			Node.set("footerContent", foot);
			Node.set("centered", true);
			Node.set("width", "33%");
			this.deleteOverlay = Node;
		},

		_createCommentNode : function(parentNode) {
			if (! this.get('commentEnabled')) return;
			var labels = this.get('uiLabels');
			var body = "<div class='annotate-comment add-comment'>";
			body += "<h3>" + labels.commentLabel + "</h3>";
			body += "<textarea class='annotate-comment-input' />";
			parentNode.one('input').insert(body, 'after');
			var commentNode = parentNode.one('.add-comment .annotate-comment-input');
			this.set('commentNode', commentNode);
			commentNode.on("key", this._onTextSubmit, 'enter', this);
		}
	});

	Y.Plugin.Annotation = Annotation;

}, '0.0.1', { requires: [
	'node','event','event-custom', 'autocomplete','overlay','recordset','io-base','json','querystring-stringify-simple'
	]
});
