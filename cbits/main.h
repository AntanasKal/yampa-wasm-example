#ifndef MAIN_H
#define MAIN_H

#include <stdint.h>
#include <stdbool.h>

void renderCircle(double posX, double posY, double radius, int colR, int colG, int colB);
void clearCanvas(int colR, int colG, int colB);
void fillStyle(int colR, int colG, int colB);
void fillRect(int x, int y, int w, int h);
int getCanvasWidth();
int getCanvasHeight();
void fillText(char* textPtr, int textLen, int x, int y, int maxWidth);
void setFont(char* textPtr, int textLen);
void arc(double x, double y, double radius, double startAngle, double endAngle, bool counterclockwise);
void ellipse(double x, double y, double radiusX, double radiusY, double rotation, double startAngle, double endAngle, bool counterclockwise);
void fill();
void beginPath();
void closePath();
void stroke();
void moveTo(double x, double y);
void lineTo(double x, double y);
#endif