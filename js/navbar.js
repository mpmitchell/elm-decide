$(document).ready(function () {
  var $header = $('#page-content-header')
  var defaultPoint = $header.position().top

  $(window).resize(function () {
    $header.css({
      top: 0
    })
    defaultPoint = $header.position().top
    move()
  })

  $(window).scroll(function () {
    move()
  })

  function move () {
    var y = $(window).scrollTop()

    if (y > defaultPoint) {
      $header.css({
        top: y - defaultPoint
      })
    } else {
      $header.css({
        top: 0
      })
    }
  }
})
