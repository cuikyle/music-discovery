import { createTheme } from "@mui/material/styles";
import typography from "./Typography";

// used for Material UI components globally
const baseTheme = createTheme({
  direction: "ltr",
  palette: {
    primary: {
      main: "#1e60b7",
      light: "#e6f4ff",
    },
    secondary: {
      main: "#1a97f5",
    },
    background: {
      default: "#fff",
    },
    success: {
      main: "#39cb7f",
      contrastText: "#ffffff",
    },
    danger: {
      main: "#fc4b6c",
    },
    error: {
      main: "#fc4b6c",
    },
    warning: {
      main: "#fdd43f",
      contrastText: "#ffffff",
    },
    text: {
      secondary: "#777e89",
      danger: "#fc4b6c",
    },
  },
  shape: {
    borderRadius: 5,
  },

  components: {
    MuiCssBaseline: {
      styleOverrides: {
        "*": {
          boxSizing: "border-box",
        },
        html: {
          height: "100%",
          width: "100%",
        },
        body: {
          height: "100%",
          margin: 0,
          padding: 0,
          overflowY: "hidden"
        },
        "#root": {
          height: "100%",
          overflowY: "hidden"
        },
      },
    },

    MuiButton: {
      styleOverrides: {
        root: {
          textTransform: "none",
          boxShadow: "none",
          "&:hover": {
            boxShadow: "none",
          },
        },
      },
    },

    MuiListItem: {
      styleOverrides: {
        root: {
          borderRadius: "9px",
        },
      },
    },

    MuiCard: {
      styleOverrides: {
        root: {
          borderRadius: "20px",
        },
      },
    },

    MuiListItemIcon: {
      styleOverrides: {
        root: {
          minWidth: "40px",
        },
      },
    },

    MuiGridItem: {
      styleOverrides: {
        root: {
          paddingTop: "30px",
          paddingLeft: "30px !important",
        },
      },
    },
  },
  status: {
    danger: "#e53e3e",
  },
  typography
});

export { baseTheme };
