#ifndef MAIN_H
#define MAIN_H

#include <stdint.h>

void renderCircle(double posX, double posY, double radius, int colR, int colG, int colB);
void clearCanvas(int colR, int colG, int colB);
void fillStyle(int colR, int colG, int colB);
void fillRect(int x, int y, int w, int h);
int getCanvasWidth();
int getCanvasHeight();
void fillText(char* textPtr, int textLen, int x, int y, int maxWidth);
void setFont(char* textPtr, int textLen);
#endif