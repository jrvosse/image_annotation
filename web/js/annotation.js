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
		target:			{ value: null }, // URI of target image to be annotated
		field:			{ value: null }, // URI identifying annotation field
		store:			{ value: null }, // URIs of web services to CRUD http annotation api
		startTyping:		{ value: null }, // timestamp when users start typing
		metatags:		{ value: {} },   // metatags dictionary
		uiLabels:		{ value: [] },   // dictionary with ui labels in the prefered language of the user
		user:                   { value: "anonymous" },

		// configuration options:
	        deleteEnabled:          { value: "mine" },   // when delete icon is shown each tag
		commentEnabled:		{ value: "always" }, // when comment icon is shown for each tag
		unsureEnabled:		{ value: "always" }, // when "I'm not sure" checkboxes will be shown for each tag
		agreeEnabled:		{ value: "yours" },  // when "I agree" checkboxes will be shown for each tag
		disagreeEnabled:	{ value: "yours" },  // when "I disagree" checkboxes will be shown for each tag
		deleteCommentEnabled:	{ value: "always" }, // when comment overlay is shown for deletions on this field
	};

	Annotation.LIST_CLASS = 'taglist';
	Annotation.LIST_TEMPLATE = '<ul class="'+Annotation.LIST_CLASS+'"></ul>';

	Y.extend(Annotation, Y.Plugin.AutoComplete, {

		initializer: function(args) {
			var parentNode = this.DEF_PARENT_NODE;

			// handler to call when item selected from autocompletion suggestions:
			this.on("select", this.onItemSelect, this);

			// handler to call when hitting return after string input (no autocomplete):
			this.get("inputNode").on("key", this.onTextSubmit, "enter", this);

			// create tags recordset (tag model in mvc), bind events to auto-update the tagList node (tag view in mvc):
			this.tags = new Y.Recordset({records:{}});
			this.tags.on("add", this.addTags, this);
			this.tags.on("update", this.updateTags, this);
			this.tags.on("remove", this.removeTags, this);

			// create tagList node (tag view in mvc)
			this.tagList = Y.Node.create(Annotation.LIST_TEMPLATE);
			parentNode.append(this.tagList);

			// infoNode is the overlay with tooltips when hovering over suggested terms
			this.infoNode = new Y.Overlay({}).render(parentNode);
			this.on("activeItemChange", this.onHover, this);
			this.on("hoveredItemChange", this.onHover, this);

			// create overlays for comments on delete and add actions:
			this.createCommentNode(parentNode);
			if (this.enabled('deleteCommentEnabled', null)) {
				Y.delegate("click", this.onTagRemoveClick, this.tagList, 'li .remove', this);
				this.createDeleteNode(parentNode);
			} else {
				Y.delegate("click", this.onDelete, this.tagList, 'li .remove', this);
			}

			// setup handlers to record typing time:
			var firstkey = true;
			this.setKeyInputHandler(firstkey);

			// everything is ready to request server for existing tags:
			this.getTags();
		},

		// handlers for adding additions, updates or removals in the tag Recordset:
		addTags : function(o) { this.renderTags(o.added, o.index); } ,
		updateTags : function(o) { this.renderTags(o.updated, o.index); },
		removeTags : function(o) {
			var tagNodes = this.tagList.all("li");
			for (var i=o.index; i < o.index+o.range; i++) {
				tagNodes.item(i).remove();
			}
		},

		renderTags : function(tags, index) {
			var tagList = this.tagList;
			var tagNodes = this.tagList.all("li");
			Y.log('rendering ' + tags.length + ' tags at index ' + index);
			if (index < tagNodes.size()) {
				this.removeTags({index:index, range:tags.length});
			}
			// format the tags
			for(var i=0; i < tags.length; i++) {
				var node = Y.Node.create('<li>'+this.formatTag(tags[i])+'</li>');
				tagList.insert(node, index+i);
				node.one('.judgeButton').detach('click');
				node.one('.commentButton').detach('click');

				node.one('.commentButton').on(           'click', this.onCommentAnnotation, this);

				node.all('.unsureButton.unchecked').on(  'click', this.onJudgeAnnotation, this, 'add', 'unsure');
				node.all('.agreeButton.unchecked').on(   'click', this.onJudgeAnnotation, this, 'add',  'agree');
				node.all('.disagreeButton.unchecked').on('click', this.onJudgeAnnotation, this, 'add',  'disagree');

				node.all('.unsureButton.checked').on(  'click', this.onJudgeAnnotation, this, 'rm', 'unsure');
				node.all('.agreeButton.checked').on(   'click', this.onJudgeAnnotation, this, 'rm', 'agree');
				node.all('.disagreeButton.checked').on('click', this.onJudgeAnnotation, this, 'rm', 'disagree');
			};
		},

		enabled : function(option, tag) {
			var when   = this.get(option);
			var user   = this.get("user");
			var author = tag?tag.getValue("user"):"no_user!";
			if (when == "always")
			  return true;
			else if (when == "never")
			  return false;
			else if (when == "mine")
			  return (user == author);
			else if (when == "yours")
			  return (user != author);

		},
		formatTag : function(tag) {
			var target= tag.getValue("target");
			var body  = tag.getValue("body");
			var label = tag.getValue("label");
			var link  = tag.getValue("display_link");
			var annot = tag.getValue("annotation");
			var meta    = this.get('metatags')[annot];
			var comment = (meta && meta.comment)?meta.comment.body.value:'';
			var screenName  = tag.getValue("screenName");

			var judgement_buttons = '';
			if (this.enabled('agreeEnabled', tag)) {
				var agreeLabel = this.get('uiLabels').agreeLabel;
				var agree_value = undefined;
				if (meta && meta.agree) agree_value = meta.agree.body.value;
				var checked = (agree_value != undefined)?'checked':'unchecked';
				judgement_buttons += "<span class='judgeButton agreeButton " + checked + "'>";
				judgement_buttons += "<img src='./icons/thumbUp.png' title='" + agreeLabel + "'/>";
				judgement_buttons += "</span>";
			}
			if (this.enabled('unsureEnabled', tag)) {
				var unsureLabel = this.get('uiLabels').unsureLabel;
				var unsure_value = undefined;
				if (meta && meta.unsure) unsure_value = meta.unsure.body.value;
				var checked = (unsure_value != undefined)?'checked':'unchecked';
				judgement_buttons += "<span class='judgeButton unsureButton " + checked + "'>";
				judgement_buttons += "<img src='./icons/unsure.png' title='" + unsureLabel + "'/>";
				judgement_buttons += "</span>";
			}
			if (this.enabled('disagreeEnabled', tag)) {
				var disagreeLabel = this.get('uiLabels').disagreeLabel;
				var disagree_value = undefined;
				if (meta && meta.disagree) disagree_value = meta.disagree.body.value;
				var checked = (disagree_value != undefined)?'checked':'unchecked';
				judgement_buttons += "<span class='judgeButton disagreeButton " + checked + "'>";
				judgement_buttons += "<img src='./icons/thumbDown.png' title='" + disagreeLabel + "'/>";
				judgement_buttons += "</span>";
			}
			if (this.enabled('commentEnabled', tag)) {
				var commentLabel = this.get('uiLabels').commentLabel;
			        judgement_buttons += "<span class='commentButton'>";
				judgement_buttons += "<img src='./icons/bubble.png' title='" + commentLabel + "'/>";
				judgement_buttons += "</span>";
			}

			var buttons = '<div class="commentButtons">' + judgement_buttons + '</div>';
			var html = '';
			html += buttons;
			html+= '<div class="screenName">'+screenName+'</div>';
			html +=	'<div class="label">';
			if (link == '')
				html += label;
			else
				html += '<a href="'+link+'">'+label+'</a>';

			if (comment && comment != "") {
			  html += ' ("' + comment +'")';
			}
			html += '</div>'
			if (this.enabled('deleteEnabled', tag)) {
			  html += '<div class="remove"><a href="javascript:{}">x</a></div>';
			}
			return html;
		},

		onTagRemoveClick : function(e) {
			var eList = this.tagList;
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

			n.one('.delete-comment-input').on("key", this.onDelete, "enter", this, annotation, index);
			n.one('#confirm-delete').on("click", this.onDelete, this, annotation, index);
			n.one('#cancel-delete').on("click", this.onCancelDelete, this);

			ov.show();
			n.one('.delete-comment-input').focus();
		},

		onCommentAnnotation : function(e) {
			var eList = this.tagList;
			var liNode = e.currentTarget.get("parentNode").get("parentNode");
			var index = this.tagList.all("li").indexOf(liNode);
			var tags = this.tags;
			var record = this.tags.getRecordByIndex(index);
			var annotation = record.getValue("annotation");
			var labels = this.get("uiLabels");
			var tag = record.getValue("label");
			var ov = this.commentOverlay;
			var n = ov.get('srcNode');

			ov.set("headerContent", "<h3 class='add_dialog'>"+ labels.commentLabel + " " + tag +"</h3>");
			n.one('.tag-comment-input').detach();
			n.one('#confirm-tag-comment').detach();
			n.one('#cancel-tag-comment').detach();

			n.one('.tag-comment-input').on("key", this.onSubmitComment, "enter", this, annotation, index);
			n.one('#confirm-tag-comment').on("click", this.onSubmitComment, this, annotation, index);
			n.one('#cancel-tag-comment').on("click", this.onCancelComment, this);

			ov.show();
			n.one('.tag-comment-input').focus();
		},

		onCancelDelete : function() {
				Y.log('onCancelDelete');
				Y.one('.delete-comment-input').detach("key", this.onDelete, "enter");
				Y.one('.delete-comment-input').set("value", "");
				Y.one('#confirm-delete').detach("click", this.onDelete);
				Y.one('#cancel-delete').detach("click", this.onCancelDelete);
				this.deleteOverlay.hide();
			     },

		onCancelComment : function() {
				Y.log('onCancelcomment');
				Y.one('.tag-comment-input').detach("key", this.onSubmitComment, "enter");
				Y.one('.tag-comment-input').set("value", "");
				Y.one('#confirm-tag-comment').detach("click", this.onSubmitComment);
				Y.one('#cancel-tag-comment').detach("click", this.onCancelComment);
				this.commentOverlay.hide();
			     },

		onDelete : function (e, annotation, index) {
			var ov = this.deleteOverlay;
			var type = "tag";
			if (ov && index >= 0) {
			    var n = ov.get('srcNode');
		            var commentNode =  n.one('.delete-comment-input');
			    var comment = commentNode.get("value");
			    commentNode.set("value", "");
			    n.one('.delete-comment-input').detach("key", this.onDelete, "enter");
			    n.one('#confirm-delete').detach("click", this.onDelete);
			    n.one('#cancel-delete').detach("click", this.onCancelDelete);
			    ov.hide();
			} else {
			    var tags = this.tags;
		            var comment = '';
			    var index = this.tagList.all("li").indexOf(e.currentTarget.get("parentNode"));
			    var record = tags.getRecordByIndex(index);
			    var annotation = record.getValue("annotation");
			}
			this.deleteAnnotation(annotation, index, comment);
		},

		deleteAnnotation : function(annotation, index, comment) {
			var oSelf = this;
			Y.log('remove annotation '+annotation+' with comment: '+comment);
			Y.io(this.get("store.remove"), {
				data:{ annotation:annotation, comment:comment },
				on:{success: function(e) {
					delete oSelf.get("metatags")[annotation];
					oSelf.tags.remove(index);
				   }
				}
			});
		},
		deleteMetaAnnotation : function(tagindex, metaindex, annotation, target, comment) {
			var oSelf = this;
			Y.log('remove meta annotation '+annotation+ ' on target ' + target +' with comment: '+comment);
			Y.io(this.get("store.remove"), {
				data:{ annotation:annotation, comment:comment },
				on:{success: function(e) {
					delete oSelf.get("metatags")[target][metaindex];
					var record = oSelf.tags.getRecordByIndex(tagindex);
					oSelf.renderTags([record], tagindex);
					Y.log("deleted " + metaindex + " for " + target);
				   }
				}
			});
		},
		getTags : function() {
			    var target = this.get('target');
			    var field = this.get('field');
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
							var ans = r[field].annotations;
							var len = ans.length;
							var metatags = oSelf.get('metatags');
							for (var i=0; i<len; i++) {
								var annotation_target = ans[i].target;
								var annotation_value = ans[i].body.value;
								var annotation_type = ans[i].type;
								if (target != annotation_target) {
									if (!metatags[annotation_target])
									  metatags[annotation_target] = {};
									if (annotation_type == "comment")
									  metatags[annotation_target][annotation_type] = ans[i];
									else
									  metatags[annotation_target][annotation_value] = ans[i];
								}
							}
							oSelf.set('metatags', metatags);

							for (var i=0; i<len; i++) {
								annotation_target = ans[i].target;
								if (target == annotation_target) {
									oSelf.tags.add(ans[i]); // normal tag
								}
							}
						  }
						}
				       }
				 }
				);
			  },
		onHover : function(e) {
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
				                      points:[Y.WidgetPositionAlign.TR, Y.WidgetPositionAlign.TL]});
				infoNode.show();
			} else {
				infoNode.hide();
			}
		},
		onItemSelect : function(e) {
			Y.log('onItemSelect');
			var type = 'tag';
			if (e.preventDefault) e.preventDefault();
			var item = e.details[0].result.raw;
			this.setKeyInputHandler(true);
			var now = new Date();
			var delta = now - this.get("startTyping");
			var target = this.get("target");
			if (item.uri && item.label) {
			  this.submitAnnotation(type, target, {type:"uri", value:item.uri }, item.label, delta);
			} else {
			  this.submitAnnotation(type, target, {type:"literal", value: item}, item,       delta);
			};
			this.get("inputNode").set("value", "");
		},

		onTextSubmit : function(e) {
			Y.log('onTextSubmit');
			Y.log(e);
			if (e.preventDefault) e.preventDefault();
			this.setKeyInputHandler(true);
			if(!this.get("activeItem")) {
			        var type = 'tag';
				var now = new Date();
				var delta = now - this.get("startTyping");
				var value = this.getTag();
				var target = this.get("target");
				this.submitAnnotation(type, target, {type:"literal", value:value}, value, delta);
			}
		},

		getTag: function() {
			      var value = this.get("inputNode").get("value");
			      this.get("inputNode").set("value", "");
			      return value?value:'';
		},

	        onJudgeAnnotation : function (ev, action, value) {
		        var eLi = ev.currentTarget.get("parentNode").get("parentNode");
			var index = this.tagList.all("li").indexOf(eLi);
			var tags = this.tags;
			var record = tags.getRecordByIndex(index);
			var target = record.getValue("annotation");
			var type = "judgement";
			var meta = this.get('metatags')[target];
			for (var prop in meta) {
				if (meta[prop].type == "judgement") {
				  var old_annotation = meta[prop].annotation;
				  this.deleteMetaAnnotation(index, prop, old_annotation, target, "overruled by new "+value+" judgement");
				}
			}
			if (action == 'add') this.submitAnnotation(type, target, {type:"literal", value: value});
		},

		onSubmitComment : function(ev, ann, index) {
			Y.log(ev);
			Y.log(ann);
			Y.log(index);
			var ov = this.commentOverlay;
			if (!ov) return;
			ov.hide();
			var type = "comment";
			var n = ov.get('srcNode');
			var commentNode =  n.one('.tag-comment-input');
			var comment = commentNode.get("value");
			commentNode.set("value", "");
			n.one('.tag-comment-input').detach("key", this.onCommentAnnotation, "enter");
			n.one('#confirm-tag-comment').detach("click", this.onCommentAnnotation);
			n.one('#cancel-tag-comment').detach("click", this.onCancelComment);
			this.submitAnnotation(type, ann, {type:"literal", value:comment});
		},

		submitAnnotation : function(type, target, body, label, timing) {
		        if (!target) return;
		        if (!body.value) return;
			if (!label) label = body.value;
			if (!timing) timing = -1;
			if (!type) type = 'default';
			Y.log('add tag: '+ body.value +' with label: '+label+ ', time: ' + timing);

			var inputNode = this.get("inputNode");
			var tags = this.tags;
			var metatags = this.get("metatags");
			var oSelf = this;

			Y.io(this.get("store.add"), {
				data:{
					field:this.get("field"),
					target:target,
					body:Y.JSON.stringify(body),
					label:label,
					typing_time: timing,
					type: type
				},
				on:{success: function(e,o) {
					var r = Y.JSON.parse(o.responseText);
					if (type == "tag")
						tags.add(r);
					else {
						var values = tags.getValuesByKey('annotation');
						var index = values.indexOf(target);
						if (!metatags[target]) metatags[target] = {}
						if (type == "judgement") {
						  metatags[target][body.value] = r;
						} else if (type == "comment") {
						  metatags[target][type] = r;
						}
						oSelf.set('metatags', metatags);
						var record = tags.getRecordByIndex(index);
						tags.update(record, index);
					}

					inputNode.focus();
				    }
				}
			});
		},

		createDeleteNode : function(parentNode) {
			var Node = new Y.Overlay({}).render(parentNode);
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

		createCommentNode : function(parentNode) {
			if (! this.get('commentEnabled')) return;
			var Node = new Y.Overlay({}).render(parentNode);
			Node.hide();
			var labels = this.get('uiLabels');
			var head = "";
			var body = "<div class='annotate-comment tag-comment'>";
			body += "<h3>" + labels.commentLabel + "</h3>";
			body += "<textarea class='annotate-comment-input tag-comment-input' />";
			var foot  = "<button id='cancel-tag-comment'>" + labels.cancelCommentLabel + "</button>";
			foot += "<button id='confirm-tag-comment'>" +labels.confirmCommentLabel+ "</button>";
			Node.set("headerContent", head);
			Node.set("bodyContent",   body);
			Node.set("footerContent", foot);
			Node.set("centered", true);
			Node.set("width", "33%");
			this.commentOverlay = Node;
		},

		// handlers to record typing time if relevant:
		setKeyInputHandler : function(first) {
			if (first) {
				this.get("inputNode").on(    "key", this.onFirstKey, 'press:', this);
			} else {
				this.get("inputNode").detach("key", this.onFirstKey, 'press:');
			}

		},
		onFirstKey : function(e) {
			if (e.button == 13) return; // ? not sure why I get the submit return here ...
			this.set("startTyping", new Date());
			var firstkey = false;
			this.setKeyInputHandler(firstkey);

		}

	});

	Y.Plugin.Annotation = Annotation;

}, '0.0.1', { requires: [
	'node','event','event-custom', 'autocomplete','overlay','recordset','io-base','json','querystring-stringify-simple'
	]
});
