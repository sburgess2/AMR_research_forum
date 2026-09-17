function annotateSlide(slide) {
  slide.querySelectorAll('.rn-auto').forEach(function (el) {
    if (!el._rnAnnotation) {
      el._rnAnnotation = RoughNotation.annotate(el, {
        type: el.dataset.rnType || 'circle',
        color: el.dataset.rnColor || '#2563eb',
        animate: true,
        animationDuration: 800,
        iterations: 2,
        padding: 4,
      });
    }
    el._rnAnnotation.show();
  });
}

Reveal.on('ready', function (event) {
  annotateSlide(event.currentSlide);
});

Reveal.on('slidechanged', function (event) {
  annotateSlide(event.currentSlide);
});
