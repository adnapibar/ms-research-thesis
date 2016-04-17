%real_distance = nan(noSensors,noSensors);

load([pwd '/traffic_data/real_dists.mat']);
load([pwd '/traffic_data/centerRoads.mat']);

noSensors = size(real_distance, 1);

for i = 1:noSensors - 1;
    for j = (i + 1):noSensors;
        
        % skip if value not NaN (don't overwrite)
        if ~isnan(real_distance(i,j))
            continue;
        end
        
        src_horz = nanmean(centerRoads(i).X); % mean might not be the best idea!
        src_vert = nanmean(centerRoads(i).Y);

        dest_horz = nanmean(centerRoads(j).X);
        dest_vert = nanmean(centerRoads(j).Y);

        [dist, failed, reason] = get_distance_gmaps_api(src_vert, src_horz, dest_vert, dest_horz);
        fprintf('Route distance is %4.2f Km between Sensor %i and %i (%s) \n',dist,i,j,reason);
       
        real_distance(i,j) = dist;
        real_distance(j,i) = dist;
    end
end

save([pwd '/traffic_data/real_dists.mat'], 'real_distance');
%% show matrix
%figure, imshow(mat2gray(real_distance)); colorbar; colormap hot;

