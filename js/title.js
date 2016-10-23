$(document).ready(function () {
  var $clone = $('.clone')
  var $header = $clone.next()
  var headerHeight = $clone.outerHeight() + 60

  $(window).resize(function () {
    if ($(window).height() <= headerHeight || $(window).width() < 790) {
      $header.css('position', 'static')
      $clone.css('display', 'none')
    } else {
      $header.css('position', 'fixed')
      $clone.css('display', 'flex')
    }
  })
})
