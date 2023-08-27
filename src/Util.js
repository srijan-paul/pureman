export const setImageSmoothing = (ctx) => (value) => {
    return () => ctx.imageSmoothingEnabled = value;
}