import React from "react"
import cx from "classnames"
import { linkVertical } from "d3-shape"
import { getX, getY, getSource, getTarget } from "./Accessors"
import { motion } from "framer-motion"

 /*
 LinkVertical.js and Accessors.js are forked directly from the VisX repository for the purposes of adding framer-motion
 for animation nodes / links.
 See:
    https://github.com/airbnb/visx/blob/master/packages/visx-shape/src/shapes/link/diagonal/LinkVertical.tsx
    https://github.com/airbnb/visx/blob/master/packages/visx-shape/src/util/accessors.ts
  */
export function pathVerticalDiagonal({ source, target, x, y }) {
    return data => {
        const link = linkVertical()
        link.x(x)
        link.y(y)
        link.source(source)
        link.target(target)
        return link(data)
    }
}

export default function LinkVerticalDiagonal({
                                                 className,
                                                 children,
                                                 data,
                                                 innerRef,
                                                 path,
                                                 x = getX,
                                                 y = getY,
                                                 source = getSource,
                                                 target = getTarget,
                                                 ...restProps
                                             }) {
    const pathGen = path || pathVerticalDiagonal({ source, target, x, y })
    if (children) return <>{children({ path: pathGen })}</>
    return (
        <motion.path
            initial={{ opacity: 0, scale: 0 }}
            animate={{ opacity: 1, scale: 1 }}
            transition={{ duration: 1 }}
            ref={innerRef}
            className={cx("visx-link visx-link-vertical-diagonal", className)}
            d={pathGen(data) || ""}
            {...restProps}
        />
    )
}
