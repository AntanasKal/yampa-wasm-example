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

void _fillRect(int, int, int, int) __attribute__((
  __import_module__("env"),
  __import_name__("fillRect")
));

void fillRect(int x, int y, int w, int h) {
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

void fillText(char* textPtr, int textLen, int x, int y, int maxWidth) {
  _fillText(textPtr, textLen, x, y, maxWidth);
}

void _setFont(char*, int) __attribute__((
  __import_module__("env"),
  __import_name__("setFont")
));

void setFont(char* textPtr, int textLen) {
  _setFont(textPtr, textLen);
}