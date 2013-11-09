# cl-gallery

cl-gallery -- is a common lisp web application, based on [RESTAS](http://restas.lisper.ru/en/) framework,
and running on [hunchentoot](http://weitz.de/hunchentoot/) server.
It is the example of utilization hunchentoot file-uploading mechanism.

## Features

* Uploader:
 *  ajax upload -- no buttons, uploading files right after selecting them.
 *  multiple files upload -- uploading several files at a time.
 *  MIME type filter -- filters the uploaded files by it's type on both client and server sides
* Gallery:
 *  Nested albums, forming any tree, you want
 *  Logic and view separation for better integration
 *  Pictures in album are arranged on the page by [masonry](http://masonry.desandro.com/) JS pluguin.
 *  Picture view is boosted by the [lightbox](http://lokeshdhakar.com/projects/lightbox2/) JS pluguin.

## Platform

Currently cl-gallery requires [sbcl](http://www.sbcl.org/), [imagemagick](http://www.imagemagick.org/script/index.php)
utility /usr/bin/convert for scaling images into thumbnails and and [exiftool](http://owl.phy.queensu.ca/~phil/exiftool/)
/usr/bin/exiftool for extracting a creation time from uploaded pictures.
Also the lightbox and masontry aren't included into the repository, you
need to install them into the `static/js` and `static/css` folders to enjoy the beautiful slideshow.
The html in the minimal version is rendered by cl-who library, easily uptained by quicklisp. You can easily use any
other html-generation library, and customize only those parts of appearance, which you need by defining corresponding
methods.

## Usage

### Uploader:
{TODO} See the example of usage in gallery.lisp
### Gallery
{TODO} See the example of usage in hello.lisp

## License
cl-gallery is licensed under [LLGPL](https://github.com/necto/cl-gallery/blob/master/COPYING) terms.

