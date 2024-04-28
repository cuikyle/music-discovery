/*
LinkVertical.js and Accessors.js are forked directly from the VisX repository for the purposes of adding framer-motion
for animation nodes / links.
See:
   https://github.com/airbnb/visx/blob/master/packages/visx-shape/src/shapes/link/diagonal/LinkVertical.tsx
   https://github.com/airbnb/visx/blob/master/packages/visx-shape/src/util/accessors.ts
 */
export function getX(l) {
    return typeof l?.x === "number" ? l?.x : 0
}

export function getY(l) {
    return typeof l?.y === "number" ? l?.y : 0
}

export function getSource(l) {
    return l?.source
}

export function getTarget(l) {
    return l?.target
}
