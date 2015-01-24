;(function() {
    
    var $ = jQuery;
    
    // inputButton
    $(document).on('click', 'a.input-button', function(e) {
        e.preventDefault()
    })

    var inputButtonInputBinding = new Shiny.InputBinding()
    $.extend(inputButtonInputBinding, {
        find: function(scope) {
            return $(scope).find(".input-button")
        },
        
        getValue: function(el) {
            // return { value: $(el).data('val') || 0, id: el.id }
            return el.id
        },
        
        setValue: function(el, value) {
            $(el).data('val', value)
        },
        
        /* 
        getType: function(el) {
            return 'shiny.action'
        },
        */
        
        subscribe: function(el, callback) {
            $(el).on("click.inputButtonInputBinding", function(e) {
                //var $el = $(this)
                //var val = $el.data('val') || 0
                //$el.data('val', val + 1)
        
                callback()
            })
        },
        
        getState: function(el) {
            return { value: this.getValue(el) }
        },
        
        receiveMessage: function(el, data) {
            
        },
        
        unsubscribe: function(el) {
            $(el).off(".inputButtonInputBinding")
        }
    });
    
    Shiny.inputBindings.register(inputButtonInputBinding, 'shiny.inputButtonInput')

})();


  