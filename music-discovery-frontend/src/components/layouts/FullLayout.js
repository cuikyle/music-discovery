import React, {useMemo, useState} from "react";
import {
    experimentalStyled,
    Container,
    Box, IconButton, useMediaQuery, useTheme,
} from "@mui/material";
import Sidebar from "./Sidebar/Sidebar";
import ArtistTree from "../tree/ArtistTree";
import {getArtistIds} from "../utils/utils";
import "../../assets/application.css"
import MenuIcon from "@mui/icons-material/Menu";
import {
    ColdplayTree,
    ColdplayArtistInfo,
    ColdplayTopTracksInfo
} from "../../assets/SampleData";

const MainWrapper = experimentalStyled("div")(({theme}) => ({}));
const PageWrapper = experimentalStyled("div")(({theme}) => ({
    backgroundColor: theme.palette.background.default,
}));

// Full Layout parent component, defining the layout of the subcomponents
const FullLayout = () => {
    const [isSidebarOpen, setIsSidebarOpen] = useState(true)
    const [rootArtist, setRootArtist] = useState(ColdplayTree.id);
    const [artistTreeData, setArtistTreeData] = useState(ColdplayTree);
    const [artistInfoData, setArtistInfoData] = useState(ColdplayArtistInfo);
    const [artistTopTracksData, setArtistTopTracksData] = useState(ColdplayTopTracksInfo);

    // uses a memo to compute the unique IDs in the tree each time the state of artistTreeData is updated
    const artistTreeIds = useMemo(() => getArtistIds(artistTreeData), [artistTreeData]);

    // show a warning if users are on a mobile device
    const isSmall = useMediaQuery(useTheme().breakpoints.down("sm"));
    if (isSmall) {
        let alerted = localStorage.getItem('alerted') || "false";
        if (alerted === "false") {
            alert("Mobile support is currently a work in progress, please also visit this website on a full-sized device.");
            localStorage.setItem("alerted", "true");
        }
    }

    return (
        <MainWrapper>

            <Sidebar
                setIsSidebarOpen={setIsSidebarOpen}
                isSidebarOpen={isSidebarOpen}
                setArtistTreeData={setArtistTreeData}
                artistInfoData={artistInfoData}
                setArtistInfoData={setArtistInfoData}
                artistTopTracksData={artistTopTracksData}
                setArtistTopTracksData={setArtistTopTracksData}
                setRootArtist={setRootArtist}
            />


            <IconButton
                color="inherit"
                aria-label="open drawer"
                edge="start"
                className={"bottom-left-nav"}
                onClick={() => setIsSidebarOpen(!isSidebarOpen)}
            >
                <MenuIcon/>
            </IconButton>

            <PageWrapper>
                <Container className={"no-margin-padding"}>
                    <Box sx={{minHeight: "calc(100vh - 170px)"}}>
                        <ArtistTree
                            key={rootArtist}
                            artistTreeData={artistTreeData}
                            artistTreeIds={artistTreeIds}
                            setArtistTreeData={setArtistTreeData}
                            setArtistInfoData={setArtistInfoData}
                            setArtistTopTracksData={setArtistTopTracksData}
                        />
                    </Box>
                </Container>
            </PageWrapper>
        </MainWrapper>
    );
};

export default FullLayout;
