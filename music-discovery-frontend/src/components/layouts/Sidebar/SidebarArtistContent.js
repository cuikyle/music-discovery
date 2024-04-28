import SearchBar from "../SearchBar";
import {Button, List, ListItem, ListItemButton, ListItemText, Typography} from "@mui/material";
import React from "react";
import {SidebarSwitch} from "./SidebarSwitch";

// Sidebar panel for artist content
export function SidebarArtistContent({artistInfoData, artistTopTracksData, setArtistTreeData, setRootArtist,
                                         setArtistInfoData, setArtistTopTracksData, setAboutArtist, showAboutArtist}) {
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
                    <img src={artistInfoData.images} className={"info-image"}/>
                    <Typography variant="h1">{artistInfoData.name}</Typography>
                </div>
                <div className={"header-margin"}>
                    <Typography variant="body1" className={"center"}>
                        {artistInfoData.name} has {artistInfoData.followers.total} followers and a
                        score of {artistInfoData.popularity} on Spotify's popularity index. Most consider their genres
                        to be:
                        {
                            artistInfoData.genres.reduce((accum, curr, i) => {
                                let isSecondLast = i === artistInfoData.genres.length - 2;
                                let isLast = i === artistInfoData.genres.length - 1;
                                return accum + curr +
                                    (isLast ? "."
                                        : isSecondLast ? ", and "
                                            : ", ");
                            }, " ")
                        }
                    </Typography>
                </div>

                <div className={"center"}>
                    <Typography variant="h4" className={"center header-margin"}>
                        {artistInfoData.name}'s top songs:
                    </Typography>
                    <List>
                        {artistTopTracksData.tracks.map((item) => (
                            <ListItem key={`${item.name + "li"}`}>
                                <ListItemButton href={`${item.href}`} target={"_blank"}
                                                key={`${item.name + "libutton"}`}>
                                    <img src={item.album.images} className={"searchbar-icon"}/>
                                    <ListItemText primary={`${item.name}`} variant="body1"/>
                                </ListItemButton>
                            </ListItem>
                        ))}

                    </List>
                    <Button variant="contained" className={"center"}
                            href={`${"https://open.spotify.com/artist/" + artistInfoData.id}`} target={"_blank"}>
                        See {artistInfoData.name} on Spotify
                    </Button>
                </div>

            </div>
        </div>
    );
}