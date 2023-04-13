import Augmentor
# image directory
img_path = '/Users/yuexuanfu/Desktop/Deep Learning/Dog-Cat-Recognition-main/DogCat-detection/validation/partially_covered'
# partially_covered
p = Augmentor.Pipeline(img_path)

p.rotate(probability=0.8, max_left_rotation=10, max_right_rotation=10)


# flipping image from left to right
p.flip_left_right(probability=0.6)

# zoom in, with probability of 0.3, and if executed, 95% of the original image will be kept and rescaled into same size
# min_factor=1.1, max_factor=1.5
p.zoom_random(probability=0.3, percentage_area=0.95)

p.skew(probability=0.8, magnitude=0.4)

#p.shear(probability=1, max_shear_left=15, max_shear_right=15)

# the number of images generated
p.sample(300)