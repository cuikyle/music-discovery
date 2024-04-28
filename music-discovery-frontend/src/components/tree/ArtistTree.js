import React from "react"
import {Group} from "@visx/group"
import {Tree, hierarchy} from "@visx/hierarchy"

import {Zoom} from "@visx/zoom"
import LinkVerticalDiagonal from "./LinkVertical";
import {Node} from "./ArtistNode";
import {useScreenSize} from "@visx/responsive";

const blue = "#1e60b7"

export const background = "#eeeeee"

const defaultMargin = {top: 0, left: 0, right: 0, bottom: 0}

// Represents a tree of nodes (ArtistNodes)
export default function ArtistTree({
                                       margin = defaultMargin, artistTreeData, artistTreeIds,
                                       setArtistInfoData, setArtistTreeData, setArtistTopTracksData
                                   }) {
    // convert the artistTreeData to a D3.js tree (hierarchy data type)
    const data = hierarchy(artistTreeData);

    const {width, height} = useScreenSize({debounceTime: 150});
    const yMax = height - margin.top - margin.bottom;
    const xMax = width - margin.left - margin.right;

    // initial positioning of the interactive SVG component for the artistTree
    const initialTransform = {
        scaleX: 1.25,
        scaleY: 1.25,
        translateX: width * 0.6,
        translateY: height * 0.2,
        skewX: 0,
        skewY: 0,
    };


    return (
        <Zoom
            width={width}
            height={height}
            scaleXMin={1 / 2}
            scaleXMax={4}
            scaleYMin={1 / 2}
            scaleYMax={4}
            initialTransformMatrix={initialTransform}
        >
            {(zoom) => ( /* zoom is the VisX interactive component */
                <svg width={width}
                     height={height}
                     style={{cursor: zoom.isDragging ? "grabbing" : "grab", touchAction: 'none'}}
                     ref={zoom.containerRef /* this is essential for the visx zoom interactions */}>

                    {/*Defining behaviors for the interactive component */}
                    <rect width={width} height={height} rx={14} fill={background}
                          onTouchStart={zoom.dragStart}
                          onTouchMove={zoom.dragMove}
                          onTouchEnd={zoom.dragEnd}
                          onMouseDown={zoom.dragStart}
                          onMouseMove={zoom.dragMove}
                          onMouseUp={zoom.dragEnd}
                          onMouseLeave={() => {
                              if (zoom.isDragging) zoom.dragEnd();
                          }}/>

                    {/* nodeSize changes size of each node */}
                    <Tree root={data} size={[yMax, xMax]} nodeSize={[120, 250]}>
                        {tree => (
                            <Group top={margin.top} left={margin.left} transform={zoom.toString()}>

                                { /* Links for the lines connecting the nodes */}
                                {tree.links().map((link, i) => (
                                    <LinkVerticalDiagonal
                                        key={`link-${i}`}
                                        data={link}
                                        stroke={blue}
                                        strokeWidth="1"
                                        fill="none"
                                    />
                                ))}

                                { /* Nodes in the tree */}
                                {tree.descendants().map((node, i) => (
                                    <Node key={`node-${i}`} node={node}
                                          artistTreeData={artistTreeData}
                                          setArtistTreeData={setArtistTreeData}
                                          artistTreeIds={artistTreeIds}
                                          setArtistInfoData={setArtistInfoData}
                                          setArtistTopTracksData={setArtistTopTracksData}
                                    />
                                ))}
                            </Group>
                        )}
                    </Tree>
                </svg>
            )}
        </Zoom>
    );
}
