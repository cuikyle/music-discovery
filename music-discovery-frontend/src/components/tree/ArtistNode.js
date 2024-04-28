import {Group} from "@visx/group";
import {motion} from "framer-motion";
import {findArtistNode, getArtistInfoData, getArtistTopTracksData, getRelatedArtists} from "../utils/utils";
import React from "react";

const blue = "#1e60b7"

// Represents one node in the tree
export function Node({
                         node,
                         artistTreeData,
                         setArtistTreeData,
                         artistTreeIds,
                         setArtistInfoData,
                         setArtistTopTracksData
                     }) {
    // Defining the sizes of each node (50px currently)
    const width = 50;
    const height = 50;
    const centerX = -width / 2;
    const centerY = -height / 2;

    return (
        <Group top={node.y} left={node.x}>
            <defs>
                <pattern
                    id={`img-${node.data.id}`}
                    patternUnits="objectBoundingBox"
                    width="100%"
                    height="100%"
                >
                    <image href={node.data.images} width={50} height={50} className={"node-icon"}/>
                </pattern>
            </defs>
            <text
                dy="-3em" // edit this to change the offset of text
                fontSize={11}
                fontFamily={'Roboto'}
                textAnchor="middle"
                fill={blue}
                style={{pointerEvents: "none", fontWeight: 700}}
            >
                {node.data.name}
            </text>
            <motion.rect
                initial={{opacity: 0, scale: 0}}
                animate={{opacity: 1, scale: 1}}
                transition={{duration: 1}}
                height={height}
                width={width}
                y={centerY}
                x={centerX}
                fill={`url(#img-${node.data.id})`}
                stroke={blue}
                strokeWidth={1}
                strokeOpacity={1}
                rx={100} // border-radius
                onClick={() => {
                    getArtistInfoData(node.data.id)
                        .then((artistInfo) => setArtistInfoData(artistInfo));

                    getArtistTopTracksData(node.data.id)
                        .then((artistTopTracksData) => setArtistTopTracksData(artistTopTracksData));

                    // Handles the case where the node is expanded meaning that the child nodes need to be minimized
                    if (node.data.isExpanded) {
                        let clonedData = {...artistTreeData};
                        let currentNode = findArtistNode(clonedData, node.data.id);
                        currentNode.children = null;
                        setArtistTreeData(clonedData);
                        currentNode.isExpanded = false;
                    } else { // Node is not expanded so related artists are requested and the tree is expanded
                        getRelatedArtists(node.data.id, artistTreeIds)
                            .then((children) => {
                                let clonedData = {...artistTreeData};
                                let currentNode = findArtistNode(clonedData, node.data.id);

                                currentNode.children = children;
                                setArtistTreeData(clonedData);
                                currentNode.isExpanded = true;
                            })
                    }
                }}
            />
        </Group>
    )
}
