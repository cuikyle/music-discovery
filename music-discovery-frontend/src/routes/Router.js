import {lazy} from "react";

const FullLayout = lazy(() => import("../components/layouts/FullLayout.js"));

// Leaving open for potentially adding more pages / functionality in the future, but currently only one route/page
const Routes = [
    {
        path: "/",
        element: <FullLayout/>,
        children: [],
    },
];

export default Routes;
