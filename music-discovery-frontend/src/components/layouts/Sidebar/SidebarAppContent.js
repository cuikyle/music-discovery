import SearchBar from "../SearchBar";
import {SidebarSwitch} from "./SidebarSwitch";
import SpotifyIcon from "../../../assets/SpotifyIcon.jpg";
import {Button, List, ListItem, ListItemButton, ListItemText, Typography, useMediaQuery, useTheme} from "@mui/material";
import {getArtistInfoData, getArtistTopTracksData, getRelatedArtists} from "../../utils/utils";
import React from "react";
import {BrunoArtistInfo, MadeonArtistInfo, TaylorArtistInfo} from "../../../assets/SampleData";

// Sidebar panel for app content
export function SidebarAppContent({showAboutArtist, setArtistTreeData, setRootArtist, setArtistInfoData,
                               setArtistTopTracksData, setAboutArtist, setIsSidebarOpen}) {
    const defaultArtists = [BrunoArtistInfo, MadeonArtistInfo, TaylorArtistInfo]
    const isSmall = useMediaQuery(useTheme().breakpoints.down("sm"));
    return (
        <div className={"background-grey"}>
            <div className={"sidepanel-inner"}>

                <SearchBar setArtistTreeData={setArtistTreeData}
                           setArtistInfoData={setArtistInfoData}
                           setArtistTopTracksData={setArtistTopTracksData}
                           setAboutArtist={setAboutArtist}
                           setRootArtist={setRootArtist}
                />

                <SidebarSwitch setAboutArtist={setAboutArtist} showAboutArtist={showAboutArtist}/>

                <div className={"center image-header-margin"}>
                    <img src={SpotifyIcon} className={"info-image"}/>
                    <Typography variant="h1">About</Typography>
                </div>
                <div className={"header-margin"}>
                    <Typography variant="body1" className={"center"}>
                        This service is aimed at reviving Spotify's music discovery tool with a modern twist.
                        Begin by searching for an artist and interacting
                        with the map (click nodes to expand, drag to move, scroll to zoom).
                    </Typography>
                </div>

                <div className={"center"}>
                    <Typography variant="h4" className={"center header-margin"}>
                        Check out these artists to get started
                    </Typography>
                    <List>
                        {defaultArtists.map((item) => (
                            <ListItem key={`${item.name + "li"}`}>
                                <ListItemButton
                                    onClick={() => {
                                        setRootArtist(item.id);

                                        getArtistInfoData(item.id)
                                            .then((artistInfo) => setArtistInfoData(artistInfo));

                                        getArtistTopTracksData(item.id)
                                            .then((artistTopTracksData) => setArtistTopTracksData(artistTopTracksData));

                                        getRelatedArtists(item.id, [])
                                            .then((relatedArtists) => {
                                                item.children = relatedArtists;
                                                item.isExpanded = true;
                                                setArtistTreeData(item);
                                                setAboutArtist(true);
                                            });

                                        if (isSmall) {
                                            setIsSidebarOpen(false);
                                        }

                                    }}
                                    key={`${item.name + "libutton"}`}>
                                    <img src={item.images} className={"searchbar-icon"}/>
                                    <ListItemText primary={`${item.name}`} variant="body1"/>
                                </ListItemButton>
                            </ListItem>
                        ))}

                    </List>
                    <Button variant="contained" className={"center"} href={`https://github.com/cuikyle/music-discovery/`} target={"_blank"}>
                        See the source code
                    </Button>

                </div>
            </div>
        </div>
    );
}