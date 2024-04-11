#lang racket/base

(require ffi/vector
         opengl
         racket/class
         racket/gui/base
         racket/math)

(define (gl-gen1 glGen)
  (u32vector-ref (glGen 1) 0))

(define (gl-create+compile-shader type src-string)
  (define src-bytes (string->bytes/utf-8 src-string))
  (define shader (glCreateShader type))
  (glShaderSource shader 1 (vector src-bytes) (s32vector (bytes-length src-bytes)))
  (glCompileShader shader)
  shader)

(define vertex-shader-source #<<GLSL
#version 330 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec4 aColor;
out vec4 vertexColor;
uniform mat4 model;
void main() {
gl_Position = model * vec4(aPos, 1.0);
vertexColor = aColor;
}
GLSL
)

(define fragment-shader-source #<<GLSL
#version 330 core
in vec4 vertexColor;
out vec4 FragColor;
void main() {
FragColor = vertexColor;
}
GLSL
)

(define cube-positions
  (f32vector
    -0.5 -0.5 -0.5
     0.5 -0.5 -0.5
     0.5  0.5 -0.5
    -0.5  0.5 -0.5
    -0.5 -0.5  0.5
     0.5 -0.5  0.5
     0.5  0.5  0.5
    -0.5  0.5  0.5))

(define cube-colors
  (f32vector
    0.0625 0.57421875 0.92578125 1.0
    0.0625 0.57421875 0.92578125 1.0
    0.0625 0.57421875 0.92578125 1.0
    0.0625 0.57421875 0.92578125 1.0
    0.52734375 0.76171875 0.92578125 1.0
    0.52734375 0.76171875 0.92578125 1.0
    0.52734375 0.76171875 0.92578125 1.0
    0.52734375 0.76171875 0.92578125 1.0))

(define cube-indices
  (u16vector
    0 1 2 2 3 0
    5 4 7 7 6 5
    7 4 0 0 3 7
    1 5 6 6 2 1
    3 2 6 6 7 3
    4 5 1 1 0 4))

(define pi 3.1416)


(define (mat4-multiply a b)
  (let ([elements
         (for*/list ([i (in-range 4)]
                     [j (in-range 4)])
           (for/fold ([sum 0.0])
                     ([k (in-range 4)])
             (+ sum (* (f32vector-ref a (+ (* i 4) k))
                       (f32vector-ref b (+ (* k 4) j))))))])
    (apply f32vector elements)))

(define (normalize v)
  (let ([len (sqrt (+ (sqr (f32vector-ref v 0))
                      (sqr (f32vector-ref v 1))
                      (sqr (f32vector-ref v 2))))])
    (f32vector (/ (f32vector-ref v 0) len)
               (/ (f32vector-ref v 1) len)
               (/ (f32vector-ref v 2) len))))

(define (subtract-vectors v1 v2)
  (f32vector (- (f32vector-ref v1 0) (f32vector-ref v2 0))
             (- (f32vector-ref v1 1) (f32vector-ref v2 1))
             (- (f32vector-ref v1 2) (f32vector-ref v2 2))))

(define (cross-product v1 v2)
  (f32vector
   (- (* (f32vector-ref v1 1) (f32vector-ref v2 2))
      (* (f32vector-ref v1 2) (f32vector-ref v2 1)))
   (- (* (f32vector-ref v1 2) (f32vector-ref v2 0))
      (* (f32vector-ref v1 0) (f32vector-ref v2 2)))
   (- (* (f32vector-ref v1 0) (f32vector-ref v2 1))
      (* (f32vector-ref v1 1) (f32vector-ref v2 0)))))

(define (dot-product v1 v2)
  (for/fold ([sum 0.0])
            ([i (in-range (f32vector-length v1))])
    (+ sum (* (f32vector-ref v1 i) (f32vector-ref v2 i)))))


(define (translate tx ty tz)
  (f32vector 1.0 0.0 0.0 tx
             0.0 1.0 0.0 ty
             0.0 0.0 1.0 tz
             0.0 0.0 0.0 1.0))

(define (scale sx sy sz)
  (f32vector sx 0.0 0.0 0.0
             0.0 sy 0.0 0.0
             0.0 0.0 sz 0.0
             0.0 0.0 0.0 1.0))

(define (rotate-y angle)
  (let ([c (cos angle)]
        [s (sin angle)])
    (f32vector c 0.0 s 0.0
               0.0 1.0 0.0 0.0
               (- s) 0.0 c 0.0
               0.0 0.0 0.0 1.0)))

(define (rotate-z angle)
  (let ([c (cos angle)]
        [s (sin angle)])
    (f32vector c (- s) 0.0 0.0
               s c 0.0 0.0
               0.0 0.0 1.0 0.0
               0.0 0.0 0.0 1.0)))

(define (perspective fov aspect near far)
  (let* ([y-scale (/ 1.0 (tan (/ fov 2.0)))]
         [x-scale (/ y-scale aspect)]
         [frustum-length (- far near)])
    (f32vector
     x-scale 0.0 0.0 0.0
     0.0 y-scale 0.0 0.0
     0.0 0.0 (- (/ (+ far near) frustum-length)) (- (/ (* 2.0 near far) frustum-length))
     0.0 0.0 -1.0 0.0)))

(define (f32map f v)
  (let* ([len (f32vector-length v)]
         [result (make-f32vector len)])
    (for ([i (in-range len)])
      (f32vector-set! result i (f (f32vector-ref v i))))
    result))

(define (look-at eye target up)
  (let* ([forward (normalize (subtract-vectors target eye))]
         [right (normalize (cross-product forward up))]
         [up-norm (cross-product right forward)]
         [neg-eye (f32map - eye)])
    (f32vector
     (f32vector-ref right 0) (f32vector-ref up-norm 0) (- (f32vector-ref forward 0)) 0.0
     (f32vector-ref right 1) (f32vector-ref up-norm 1) (- (f32vector-ref forward 1)) 0.0
     (f32vector-ref right 2) (f32vector-ref up-norm 2) (- (f32vector-ref forward 2)) 0.0
     (dot-product right neg-eye) (dot-product up-norm neg-eye) (dot-product forward neg-eye) 1.0)))

(define cube-canvas%
  (class canvas%
    (inherit with-gl-context swap-gl-buffers)

    (define gl-config (new gl-config%))
    (send gl-config set-legacy? #f)
    (super-new [style '(gl no-autoclear)]
               [gl-config gl-config])
    (define rotation-angle 45.0)
    (define timer
      (new timer%
           [notify-callback
            (lambda ()
              (set! rotation-angle (if (>= (+ rotation-angle 0.01) (* 2 pi))
                                       (- (+ rotation-angle 0.01) (* 2 pi))
                                       (+ rotation-angle 0.01)))
              (send this refresh))]))
    
    (send timer start 16 #t)

    (define-values [program position-buffer color-buffer index-buffer]
      (with-gl-context
        (λ ()
          (glBindVertexArray (gl-gen1 glGenVertexArrays))

          (define vertex-shader (gl-create+compile-shader GL_VERTEX_SHADER vertex-shader-source))
          (define fragment-shader (gl-create+compile-shader GL_FRAGMENT_SHADER fragment-shader-source))

          (define program (glCreateProgram))
          (glAttachShader program vertex-shader)
          (glAttachShader program fragment-shader)
          (glLinkProgram program)

          (glDetachShader program vertex-shader)
          (glDetachShader program fragment-shader)
          (glDeleteShader vertex-shader)
          (glDeleteShader fragment-shader)

          (define position-buffer (gl-gen1 glGenBuffers))
          (glBindBuffer GL_ARRAY_BUFFER position-buffer)
          (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof cube-positions) cube-positions GL_STATIC_DRAW)

          (define color-buffer (gl-gen1 glGenBuffers))
          (glBindBuffer GL_ARRAY_BUFFER color-buffer)
          (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof cube-colors) cube-colors GL_STATIC_DRAW)

          (define index-buffer (gl-gen1 glGenBuffers))
          (glBindBuffer GL_ELEMENT_ARRAY_BUFFER index-buffer)
          (glBufferData GL_ELEMENT_ARRAY_BUFFER (gl-vector-sizeof cube-indices) cube-indices GL_STATIC_DRAW)

          (values program position-buffer color-buffer index-buffer))))

    (define/override (on-paint)
      (with-gl-context
        (λ ()
          (glClearColor 0.0 0.0 0.0 0.0)
          (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

          (define modelLoc (glGetUniformLocation program "model"))
          (define initial-x-rotation (rotate-z (/ (* 30 pi) 180)))
          (define rotation-y-matrix (rotate-y rotation-angle))
          (define model-matrix (mat4-multiply initial-x-rotation rotation-y-matrix))
          (glUseProgram program)
          (glUniformMatrix4fv modelLoc 1 #f model-matrix)

          (glEnableVertexAttribArray 0)
          (glBindBuffer GL_ARRAY_BUFFER position-buffer)
          (glVertexAttribPointer 0 3 GL_FLOAT #f 0 0)

          (glEnableVertexAttribArray 1)
          (glBindBuffer GL_ARRAY_BUFFER color-buffer)
          (glVertexAttribPointer 1 4 GL_FLOAT #f 0 0)

          (glBindBuffer GL_ELEMENT_ARRAY_BUFFER index-buffer)
          (glDrawElements GL_TRIANGLES (u16vector-length cube-indices) GL_UNSIGNED_SHORT 0)

          (glDisableVertexAttribArray 0)
          (glDisableVertexAttribArray 1)
          (send timer start 16)
          (swap-gl-buffers))))))

(define (show-frame label cube-canvas%)
  (define frame (new frame% [label label] [width 300] [height 300]))
  (new cube-canvas% [parent frame])
  (send frame show #t))

(show-frame "Cube" cube-canvas%)
