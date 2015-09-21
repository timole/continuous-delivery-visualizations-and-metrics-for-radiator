class Dashing.Cdinfo extends Dashing.Widget

  @accessor 'oldestDoneFeature', Dashing.AnimatedValue

  constructor: ->
    super
    @observe 'oldestDoneFeature', (oldestDoneFeature) ->
      $(@node).find(".cdinfo").val(oldestDoneFeature).trigger('change')

  ready: ->
    cdinfo = $(@node).find(".cdinfo")
    cdinfo.attr("data-bgcolor", "#1AAA1A")
    cdinfo.attr("data-fgcolor", cdinfo.css("color"))
    cdinfo.knob()

  onData: (data) ->
    if data.oldestDoneFeature > 7
      $(@node).css("background-color", "red")
    else
      $(@node).css("background-color", "green")

