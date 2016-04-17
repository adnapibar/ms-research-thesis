function [ distance, fail, reason ] = get_distance_gmaps_api( orig_lat, orig_long, dest_lat, dest_long)
%GET_DISTANCE_GMAPS_API returns L1 (cityblock) actual shortest path
% distance on maps from longitude and latitude coordinates between
% a source and a target destination.
    
    distance = NaN;
    fail = 0;
    reason = [];
    
    mode = 'driving';
    
    orig_lat = num2str(orig_lat);
    orig_long = num2str(orig_long);
    dest_lat = num2str(dest_lat);
    dest_long = num2str(dest_long);
    
    theurl = ['https://maps.googleapis.com/maps/api/distancematrix/json?origins=(', ...
            orig_lat, ',' ,orig_long, ')&destinations=(', ...
            dest_lat, ',' ,dest_long, ')&mode=', mode, '&language=en-EN&sensor=false', ...
            '&key=AIzaSyBl6SUiQYRRwq52Leyn3xfd15oxS4IHg9M'];
    % you should generate your own maps api key and replace it int he above line
        
    [string, success] = urlread(theurl);

    if ~success
        warning('Could not retrieve JSON file from Google Maps API V3!');
        fail = 1;
        reason = 'URL_RETRIEVE_FAIL';
        return;
    end

    parsed = JSON.parse(string);
    reason = parsed.status;

    if isfield(parsed,'error_message')
        error_msg = parsed.error_message;
    else
        error_msg = reason;
    end
    
    if strcmp(reason, 'OK') && strcmp(parsed.rows{1,1}.elements{1,1}.status, 'OK') == 1
        distance = parsed.rows{1,1}.elements{1,1}.distance.value * 0.001; % convert to km
    else
        warning('Could not parse JSON file to matlab struct.\n Reason: %s', error_msg);
        fail = 1;
    end
    
end
