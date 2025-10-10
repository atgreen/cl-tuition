# Spring Physics in Tuition

Tuition includes a spring physics animation library ported from [Harmonica](https://github.com/charmbracelet/harmonica), which itself is based on [Ryan Juckett's damped harmonic oscillator](https://www.ryanjuckett.com/damped-springs/).

## What is Spring Physics?

Spring physics creates smooth, natural motion by simulating a mass-spring-damper system. Unlike linear or eased animations, spring animations:

- Feel **natural and responsive**
- Can **change direction mid-animation** smoothly
- **Automatically handle interruptions** without jank
- Are used by Apple (iOS), Google (Material Design), and modern web frameworks

## Quick Start

```lisp
;; Create a spring (60 FPS, angular frequency 6.0, damping 0.5)
(defvar *spring* (tui:make-spring-animation (tui:fps 60) 6.0d0 0.5d0))

;; Initialize position and velocity
(defvar *x* 0.0d0)
(defvar *x-velocity* 0.0d0)
(defvar *target* 100.0d0)

;; In your update loop (called ~60 times per second):
(multiple-value-setq (*x* *x-velocity*)
  (tui:spring-update *spring* *x* *x-velocity* *target*))
```

## Damping Ratios

The damping ratio controls oscillation behavior:

### Under-Damped (< 1.0) - Bouncy
```lisp
(make-spring-bouncy)  ; Pre-configured bouncy spring
```
- Fast movement
- Overshoots target
- Oscillates before settling
- Good for: playful UIs, attention-grabbing animations

### Critically-Damped (= 1.0) - Smooth
```lisp
(make-spring-smooth)  ; Pre-configured smooth spring
```
- Fast movement
- No overshoot
- No oscillation
- Good for: scrolling, menus, general UI movement

### Over-Damped (> 1.0) - Gentle
```lisp
(make-spring-gentle)  ; Pre-configured gentle spring
```
- Slow movement
- No overshoot
- No oscillation
- Good for: subtle animations, fade-ins

## API Reference

### Core Functions

#### `(fps n)`
Convert frames per second to time delta.
```lisp
(fps 60)  ; => 0.016666... (seconds per frame)
```

#### `(make-spring-animation delta-time angular-frequency damping-ratio)`
Create a spring with custom parameters.

**Parameters:**
- `delta-time` - Time step (use `fps` helper or provide seconds per frame)
- `angular-frequency` - Speed (higher = faster), typically 1.0-10.0
- `damping-ratio` - Oscillation behavior (see above)

**Returns:** A `spring` structure

#### `(spring-update spring position velocity target-position)`
Update position and velocity toward target using spring physics.

**Parameters:**
- `spring` - Spring created with `make-spring-animation`
- `position` - Current position
- `velocity` - Current velocity
- `target-position` - Where the spring wants to be

**Returns:** `(values new-position new-velocity)`

### Convenience Functions

Pre-configured springs for common use cases:

- `(make-spring-smooth &optional fps)` - Critically-damped (no overshoot)
- `(make-spring-bouncy &optional fps)` - Under-damped (bouncy)
- `(make-spring-gentle &optional fps)` - Over-damped (slow and smooth)
- `(make-spring-snappy &optional fps)` - Fast and responsive

All default to 60 FPS.

## Usage Examples

### Smooth Scrolling
```lisp
(defclass my-model ()
  ((scroll-pos :initform 0.0d0)
   (scroll-vel :initform 0.0d0)
   (scroll-target :initform 0.0d0)
   (spring :initform (tui:make-spring-smooth))))

(defmethod update ((m my-model) msg)
  ;; Update scroll position with spring
  (with-accessors ((pos scroll-pos) (vel scroll-vel)
                   (target scroll-target) (spring spring)) m
    (multiple-value-setq (pos vel)
      (tui:spring-update spring pos vel target)))
  ...)
```

### Animated Selection Cursor
```lisp
(defclass list-model ()
  ((cursor-y :initform 0.0d0)
   (cursor-vel :initform 0.0d0)
   (selected-index :initform 0)
   (spring :initform (tui:make-spring-snappy))))

(defmethod update ((m list-model) msg)
  (when (arrow-key-pressed-p msg)
    ;; Update selected index, spring animates cursor
    (incf (selected-index m)))

  ;; Animate cursor to selected position
  (with-accessors ((y cursor-y) (vel cursor-vel)
                   (idx selected-index) (spring spring)) m
    (let ((target-y (* idx 2.0d0))) ; 2 lines per item
      (multiple-value-setq (y vel)
        (tui:spring-update spring y vel target-y))))
  ...)
```

### Counter Animation
```lisp
(defclass counter-model ()
  ((display-value :initform 0.0d0)
   (value-vel :initform 0.0d0)
   (actual-value :initform 0)
   (spring :initform (tui:make-spring-smooth))))

(defmethod view ((m counter-model))
  ;; Animate displayed value toward actual value
  (with-accessors ((display display-value) (vel value-vel)
                   (actual actual-value) (spring spring)) m
    (multiple-value-setq (display vel)
      (tui:spring-update spring display vel (float actual 1.0d0))))

  (format nil "Count: ~D" (round display)))
```

## Performance Notes

- Springs are **very efficient** - just a few floating-point operations per update
- Coefficients are pre-computed when you create the spring
- Use the same spring instance for multiple values (e.g., x and y coordinates)
- All math uses double-floats for precision

## See Also

- `examples/spring-animation.lisp` - Interactive demonstration
- [Harmonica](https://github.com/charmbracelet/harmonica) - Original Go implementation
- [Ryan Juckett's article](https://www.ryanjuckett.com/damped-springs/) - Math explanation
