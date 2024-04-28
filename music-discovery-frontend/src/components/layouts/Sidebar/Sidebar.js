import React, {useState} from "react";
import {Drawer} from "@mui/material";
import {SidebarArtistContent} from "./SidebarArtistContent";
import {SidebarAppContent} from "./SidebarAppContent";

// Parent sidebar component
function Sidebar({setArtistTreeData, artistInfoData, setArtistInfoData, artistTopTracksData,
                     setArtistTopTracksData, isSidebarOpen, setIsSidebarOpen, setRootArtist}) {
    const [showAboutArtist, setAboutArtist] = useState(false);

    return (
        <div className={"set-behind-nav"}>
            <Drawer
                anchor="left"
                open={isSidebarOpen}
                variant="persistent"
                PaperProps={{
                    sx: {
                        width: {xs: "100%", sm: "50%", md: "20%"}
                    },
                }}
            >
                {showAboutArtist ?
                    <SidebarArtistContent
                        artistInfoData={artistInfoData}
                        artistTopTracksData={artistTopTracksData}
                        showAboutArtist={showAboutArtist}
                        setAboutArtist={setAboutArtist}
                        setArtistTreeData={setArtistTreeData}
                        setArtistInfoData={setArtistInfoData}
                        setArtistTopTracksData={setArtistTopTracksData}
                        setRootArtist={setRootArtist}
                    />
                    :
                    <SidebarAppContent
                        showAboutArtist={showAboutArtist}
                        setAboutArtist={setAboutArtist}
                        setArtistTreeData={setArtistTreeData}
                        setArtistInfoData={setArtistInfoData}
                        setArtistTopTracksData={setArtistTopTracksData}
                        setRootArtist={setRootArtist}
                        setIsSidebarOpen={setIsSidebarOpen}
                    />
                }
            </Drawer>
        </div>
    );

}

export default Sidebar;
