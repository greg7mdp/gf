// ------------------------------------------------------------------------------------------
// From: https://github.com/sherwinvishesh/ASCII-Donut-Animation
//
// MIT License
//
// Copyright (c) 2024 Sherwin Vishesh Jathanna
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
// ------------------------------------------------------------------------------------------

#include <iostream>
#include <string>
#include <cmath>
#include <unistd.h>
#include <vector>

using namespace std;

// Function to set the color based on user input
string setColor(string color) {
    if (color == "red") {
        return "\033[31m";
    } else if (color == "green") {
        return "\033[32m";
    } else if (color == "yellow") {
        return "\033[33m";
    } else if (color == "blue") {
        return "\033[34m";
    } else if (color == "magenta") {
        return "\033[35m";
    } else if (color == "cyan") {
        return "\033[36m";
    } else if (color == "white") {
        return "\033[37m";
    }
    // Default to white if no valid color is chosen
    return "\033[37m";
}

int main() {
    float A = 0, B = 0, i, j;
    int size = 5;
    string colorInput {"blue"};

    string color = setColor(colorInput); // Set color

    float R1 = 1, R2 = 2, K2 = 5;
    // Adjust R2 based on the size input
    R2 = R2 + (size - 5) * 0.2; // Scale R2 based on input size
    float K1 = 10 / (R1 + R2);
    vector<char> b(1760, ' ');
    vector<float> z(1760, 0);
    cout << "\x1b[2J";
    for (;;) {
        fill(b.begin(), b.end(), ' ');
        fill(z.begin(), z.end(), 0);
        for (j = 0; 6.28 > j; j += 0.07)
            for (i = 0; 6.28 > i; i += 0.02) {
                float c = sin(i), d = cos(j), e = sin(A), f = sin(j),
                      g = cos(A), h = d + 2, D = 1 / (c * h * e + f * g + 5),
                      l = cos(i), m = cos(B), n = sin(B),
                      t = c * h * g - f * e;
                int x = 40 + 30 * D * (l * h * m - t * n),
                    y = 12 + 15 * D * (l * h * n + t * m),
                    o = x + 80 * y,
                    N = 8 * ((f * e - c * d * g) * m - c * d * e - f * g - l * d * n);
                if (22 > y && y > 0 && x > 0 && 80 > x && D > z[o]) {
                    z[o] = D;
                    b[o] = ".,-~:;=!*#$@"[N > 0 ? N : 0];
                }
            }
        cout << "\x1b[H";
        for (int k = 0; 1761 > k; k++)
            if (k % 80) {
                cout << color << b[k];
            } else {
                cout << endl;
            }
        A += 0.04;
        B += 0.02;
        usleep(30000);
    }
    return 0;
}