# bird

https://user-images.githubusercontent.com/64008205/152782577-84d08f6e-6cf9-4d36-8424-34732a87cc76.mp4

bird is a copy of Flappy Bird. 
I used the assets of flappy-haskell.

## Install 

1.stack 

```bash 
$ curl -sSL https://get.haskellstack.org/ | sh
```
or :

```bash 
wget -q0 -https://get.haskellstack.org/ | sh 
```
On Windows, you can download and install the Windows64-bit Installer.


2.freeglut (Windows)

Use this.
http://files.transmissionzero.co.uk/software/development/GLUT/freeglut-MinGW.zip
Copy "freeglut/bin/(platform)/freeglut.dll" to your executable path and 
rename it to "glut32.dll"


## Usage

```bash
$ git clone https://github.com/Kohei-Wada/bird.git
$ cd bird
$ stack run 
``` 
