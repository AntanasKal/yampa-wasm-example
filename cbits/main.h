#ifndef MAIN_H
#define MAIN_H

#include <stdint.h>

void renderCircle(double posX, double posY, double radius, int colR, int colG, int colB);
void clearCanvas(int colR, int colG, int colB);
void fillText(char* textPtr, int textLen, int x, int y, int maxWidth);
#endif