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
void _fillText(char*, int, int, int, int) __attribute__((
  __import_module__("env"),
  __import_name__("fillText")
));

void fillText(char* textPtr, int textLen, int x, int y, int maxWidth) {
  _fillText(textPtr, textLen, x, y, maxWidth);
}