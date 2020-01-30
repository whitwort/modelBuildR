;(function() {
    
    function render(el) {
        var widget = $(el)
        
        var template = widget.data('template')
        var html     = template(widget.data('context'))
        
        $("#" + el.id).empty().append(html)
        
    }
    
    /* Custom message handlers to add or remove CSS classes */
    Shiny.addCustomMessageHandler('addClass', function(message) {
        $(message.selector).addClass(message.class)
    })
    Shiny.addCustomMessageHandler('removeClass', function(message) {
        $(message.selector).removeClass(message.class)
    })
    
    /* buttonDropdownInput */
    var dropdownInputBinding = new Shiny.InputBinding()
    $.extend(dropdownInputBinding, {
        
        find: function(scope) {
            return $(scope).find(".dropdown-input") 
        },
        
        initialize: function(el) {
            var widget = $(el)
            widget.data("value", null)
            
            widget.on("click", "a", function(event) {
                widget.data('value', $(event.target).attr('value'))
                
                widget.trigger("change")
                event.preventDefault()
            })
        },
        
        getValue: function(el) {
            return $(el).data('value')
        },
        
        setValue: function(el, value) {
            $(el).data('value', value)
        },
        
        subscribe: function(el, callback) {
            $(el).on("change.dropdownInputBinding", function(e) {
                callback() 
            })
        },
        
        unsubscribe: function(el) {
            $(el).off(".dropdownInputBinding") 
        },
        
        getType: function(el) {
            return 'modelBuildR.dropdown-input'
        }    
    
    })
    Shiny.inputBindings.register(dropdownInputBinding, "dropdown-input")
    
    /* vectorInput */
    var vectorInputBinding = new Shiny.InputBinding()
    $.extend(vectorInputBinding, {
    
        find: function(scope) {
            return $(scope).find(".vector-input") 
        },
    
        initialize: function(el) {
            var widget = $(el)
            
            widget.data( 'template', Handlebars.templates['named-vector'] )
            widget.data( 'context'
                       , { id:          widget.attr('id')
                         , type:        widget.attr('type')
                         , nameLabel:   widget.attr('nameLabel')
                         , valueLabel:  widget.attr('valueLabel')
                         , values:      widget.attr('values') || []
                         }
                       )
                       
            // Browser event binding   
            widget.on("click", "button", function(event) {
                var target = event.target.id.split("-")
                var type = target[0], id = target[1], index = target[2]
                
                if (type === 'add') {
                    widget.data('context').values.push({name: "Name", value: "0"})
                } else if (type == 'del') {
                    widget.data('context').values.splice(index, 1)
                }
                
                widget.trigger("change")
                render(el)
                
                event.preventDefault()
            })
            
            widget.on("keyup", "input", function(event) {
                var target = event.target.id.split("-")
                var id = target[0], index = target[1], type = target[2]
                
                widget.data('context').values[index][type] = event.target.value
                widget.trigger("change")
                
                return true
            })
                       
            render(el)
        },
        
        getValue: function(el) {
            return $(el).data('context').values
        },
        
        receiveMessage: function(el, data) {
            // Incomming value will be a named object
            var values = []
            for (var k in data.value) {
                values.push({name: k, value: data.value[k]})
            }
            
            this.setValue(el, values)
        },
        
        setValue: function(el, value) {
            var widget = $(el)
            
            widget.data('context').values = value
            render(el)
            
            widget.trigger('change')
        },
        
        subscribe: function(el, callback) {
            $(el).on("change.vectorInputBinding", function(e) { 
                callback() 
            })
        },
        
        unsubscribe: function(el) {
            $(el).off(".vectorInputBinding") 
        },
        
        getType: function(el) {
            return 'modelBuildR.vector-input'
        }
    
    })
    Shiny.inputBindings.register(vectorInputBinding, "vector-input")

    /* equationEditor */
    var updateEditor = function(el) {
        var widget = $(el)
        render(el)
        
        var selects = widget.find('select').selectize({hideSelected: false, enableDuplicate: true, duplicates: true})
        for (var i = 0; i < selects.length; i++) {
            selects[i].selectize.setValue(widget.data('context').values[i] || [])
        }
    }
    
    var equationEditorBinding = new Shiny.InputBinding()
    $.extend(equationEditorBinding, {
    
        find: function(scope) {
            return $(scope).find(".equation-editor") 
        },
        
        initialize: function(el) {
            var widget = $(el)
            
            widget.data( 'template', Handlebars.templates['equation-editor'] )
            widget.data( 'context'
                       , { id:          widget.attr('id')
                         , math:        widget.attr('math').split(",")
                         , values:      widget.attr('values') || []
                         , description: widget.attr('description') || ""
                         }
                       )
            
            // Browser event binding            
            var mF = function(o) { return o.name }
            $('.vector-input').on("change.vectorInputBinding", function(e) {
                widget.data('context').states     = $('#states').data('context').values.map(mF)
                widget.data('context').parameters = $('#parameters').data('context').values.map(mF)
                
                updateEditor(el)
            })
            
            widget.on("change", "select", function(event) {
                var index = event.target.id.split("-")[1]
                
                widget.data('context').values[index] = event.target.selectize.getValue()
                widget.trigger("change")
                
                widget.find(".item[data-value^='parameters']").addClass("bg-info")
                widget.find(".item[data-value^='states']").addClass("bg-success")
            })
            
        },
        
        getValue: function(el) {
            var widget = $(el)
            return widget.data('context').values
        },
        
        receiveMessage: function(el, data) {
            this.setValue(el, data.value)
        },
        
        setValue: function(el, values) {
            var widget = $(el)
            
            widget.data('context').values = values
            updateEditor(el)
            
            widget.trigger('change')
        },
        
        subscribe: function(el, callback) {
            $(el).on("change.equationEditorBinding", function(e) { 
                callback()
            })
        },
        
        unsubscribe: function(el) {
            $(el).off(".equationEditorBinding") 
        },
        
        getType: function(el) {
            return 'modelBuildR.equation-editor'
        }
    
    })
    Shiny.inputBindings.register(equationEditorBinding, "equation-editor")

    var equationEditorOutput = new Shiny.OutputBinding()
    $.extend(equationEditorOutput, {
        
        find: function(scope) {
            return $(scope).find(".equation-editor") 
        },
        
        renderValue: function(el, data) {
            for (var i = 0; i < data.length; i++) {
                if (data[i]) { 
                    $('#equation-container-' + i).removeClass('has-error')
                } else {
                    $('#equation-container-' + i).addClass('has-error')
                }
            }
        },
        
        renderError: function(el, err) {
            console.log(err)
        },
        
        clearError: function(el) {
            
        }
    
    })
    Shiny.outputBindings.register(equationEditorOutput, "equation-editor")

})();
