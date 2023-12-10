#include <stdint.h>
#include <main.h>

void _renderCircle(double, double, double, int, int, int) __attribute__((
  __import_module__("env"),
  __import_name__("renderCircle")
));

void renderCircle(double posX, double posY, double radius, int colR, int colG, int colB) {
  _renderCircle(posX, posY, radius, colR, colG, colB);
}

void _clearCanvas(int, int, int) __attribute__((
  __import_module__("env"),
  __import_name__("clearCanvas")
));

void clearCanvas(int colR, int colG, int colB) {
  _clearCanvas(colR, colG, colB);
}

void _fillStyle(int, int, int) __attribute__((
  __import_module__("env"),
  __import_name__("fillStyle")
));

void fillStyle(int colR, int colG, int colB) {
  _fillStyle(colR, colG, colB);
}

void _fillRect(double, double, double, double) __attribute__((
  __import_module__("env"),
  __import_name__("fillRect")
));

void fillRect(double x, double y, double w, double h) {
  _fillRect(x, y, w, h);
}

int _getCanvasWidth() __attribute__((
  __import_module__("env"),
  __import_name__("getCanvasWidth")
));

int getCanvasWidth() {
  return _getCanvasWidth();
}

int _getCanvasHeight() __attribute__((
  __import_module__("env"),
  __import_name__("getCanvasHeight")
));

int getCanvasHeight() {
  return _getCanvasHeight();
}

void _fillText(char*, int, int, int, int) __attribute__((
  __import_module__("env"),
  __import_name__("fillText")
));

void fillText(char* textPtr, int textLen, double x, double y, double maxWidth) {
  _fillText(textPtr, textLen, x, y, maxWidth);
}

void _setFont(char*, int) __attribute__((
  __import_module__("env"),
  __import_name__("setFont")
));

void setFont(char* textPtr, int textLen) {
  _setFont(textPtr, textLen);
}


void _arc(double, double, double, double, double, bool) __attribute__((
  __import_module__("env"),
  __import_name__("arc")
));
void arc(double x, double y, double radius, double startAngle, double endAngle, bool counterclockwise) {
  _arc(x, y, radius, startAngle, endAngle, counterclockwise);
}
void _ellipse(double, double, double, double, double, double, double, bool) __attribute__((
  __import_module__("env"),
  __import_name__("ellipse")
));
void ellipse(double x, double y, double radiusX, double radiusY, double rotation, double startAngle, double endAngle, bool counterclockwise) {
  _ellipse(x, y, radiusX, radiusY, rotation, startAngle, endAngle, counterclockwise);
}
void _fill() __attribute__((
  __import_module__("env"),
  __import_name__("fill")
));
void fill() {
  _fill();
}
void _beginPath() __attribute__((
  __import_module__("env"),
  __import_name__("beginPath")
));
void beginPath() {
  _beginPath();
}
void _closePath() __attribute__((
  __import_module__("env"),
  __import_name__("closePath")
));
void closePath() {
  _closePath();
}
void _stroke() __attribute__((
  __import_module__("env"),
  __import_name__("stroke")
));
void stroke() {
  _stroke();
}
void _moveTo(double, double) __attribute__((
  __import_module__("env"),
  __import_name__("moveTo")
));
void moveTo(double x, double y) {
  _moveTo(x, y);
}
void _lineTo(double, double) __attribute__((
  __import_module__("env"),
  __import_name__("lineTo")
));
void lineTo(double x, double y) {
  _lineTo(x, y);
}
