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
 *  albums
 *  logic and view separation for better integration
 *  Picture view is boosted by the [lightbox](http://lokeshdhakar.com/projects/lightbox2/) JS pluguin.

## Platform

Currently cl-gallery requires [sbcl](http://www.sbcl.org/) and [imagemagick](http://www.imagemagick.org/script/index.php)
utility /usr/bin/convert for scaling images into thumbnails. Also the lightbox isn't included into the repository, you
need to install it into the `static/js` and `static/css` folders to enjoy the beautiful slideshow. The html is rendered
by cl-who library, easily uptained by quicklisp.

## Usage

### Uploader:
{TODO} See the example of usage in gallery.lisp
### Gallery
{TODO} See the example of usage in hello.lisp

## License
cl-gallery is licensed under [LLGPL](https://github.com/necto/cl-gallery/blob/master/COPYING) terms.

